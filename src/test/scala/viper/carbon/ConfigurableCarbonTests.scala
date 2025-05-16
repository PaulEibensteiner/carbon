package viper.carbon

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Path, Paths => JPaths}
import org.scalatest.ConfigMap
import viper.silver.reporter.NoopReporter
import viper.silver.testing.{ProjectInfo, SystemUnderTest, AnnotatedTestInput, AbstractOutput, SilOutput}
import viper.silver.utility.{Paths, TimingUtils}
import viper.silver.verifier.{AbstractError, AbstractVerificationError, Failure, Success, Verifier}
import viper.silver.testing.SilSuite
import viper.silver.frontend.Frontend
import viper.silver.logger.SilentLogger
import scala.util.parsing.json.JSON

class ConfigurableCarbonTests extends SilSuite {

    /** Following a hyphenation-based naming scheme is important for handling project-specific annotations. */
    def name = "Carbon-Statistics"

    // The only system property - path to the config file
    private val configFilePathProperty = "CARBON_CONFIG_FILE"
    
    // Default configuration values - all in one place for easy overview
    private object Defaults {
        val repetitions: Int = 1
        val warmupLocation: Option[String] = None
        val targetLocation: Option[String] = None  // Required, no default
        val csvFile: Option[String] = None
        val inclusionFile: Option[String] = None
        val carbonArguments: Seq[String] = Seq(
            "--timeout=600",
            "--boogieOpt=/vcsCores:1",
            "--assumeInjectivityOnInhale",
            "--useOldAxiomatization"
        )
    }
    
    // Config structure parsed from JSON
    private lazy val config: Map[String, Any] = {
        val configFilePath = Option(System.getProperty(configFilePathProperty))
            .getOrElse(throw new IllegalArgumentException(s"System property '$configFilePathProperty' must be set"))

        val jsonString = new String(Files.readAllBytes(JPaths.get(configFilePath)))
        JSON.parseFull(jsonString) match {
            case Some(parsed: Map[_, _]) => 
                parsed.map { case (k, v) => (k.toString, v) }
            case _ => 
                throw new IllegalArgumentException("Invalid JSON configuration file")
        }
    }

    // Centralized configuration access methods
    protected def repetitions: Int = {
        val reps = config.getOrElse("repetitions", Defaults.repetitions).toString.toDouble.toInt
        failIf(s"Repetitions must be >= 1, but got $reps", reps < 1)
        reps
    }

    protected def warmupDirName: Option[String] = {
        val dir = config.getOrElse("warmupLocation", "").toString.trim
        if (dir.isEmpty) None else Option(dir)
    }

    protected def targetDirName: String = 
        config.get("targetLocation") match {
            case Some(name) => name.toString
            case None => fail("'targetLocation' not specified in config file")
        }

    protected def csvFileName: Option[String] = getConfigStringOption("csvFile")
    protected def inclusionFileName: Option[String] = getConfigStringOption("inclusionFile")

    private var csvFile: BufferedWriter = _
    private var testsToInclude: Option[Set[String]] = None

    // Carbon arguments constructed from config
    lazy val carbonArguments: Seq[String] = {
        // Get custom arguments from config, or use defaults
        config.get("carbonArguments") match {
            case Some(args: List[_]) => args.map(_.toString)
            case _ => Defaults.carbonArguments
        }
    }

    val randomization: Option[(Seq[String], String, Int => Int)] = {
        Some(carbonArguments, "--proverSpecificRandomSeed", i => i)
    }

    // Verifier configuration
    lazy val verifier: CarbonVerifier = {
        val reporter = NoopReporter
        val carbon = CarbonVerifier(reporter)
        carbon.parseCommandLine(carbonArguments ++ Seq("dummy.vpr"))
        carbon
    }

    override def verifiers = Vector(verifier)

    // Test directory methods
    override def testDirectories: Seq[String] = warmupDirName match {
        case Some(w) => Vector(w, targetDirName)
        case None => Vector(targetDirName)
    }

    override def getTestDirPath(testDir: String): Path = {
        failIf(s"Test directory path cannot be null", testDir == null)
        
        // Use JPaths to properly handle "../" in paths
        val path = JPaths.get(testDir).toAbsolutePath().normalize()
        val targetFile = path.toFile
        
        // Log working directory and resolved path for debugging
        info(s"Working directory: ${new File(".").getAbsolutePath}")
        info(s"Resolved test directory: ${targetFile.getAbsolutePath}")
        
        // Check that the directory exists
        failIf(s"Test directory does not exist: ${targetFile.getAbsolutePath}", !targetFile.exists())
        failIf(s"Path is not a directory: ${targetFile.getAbsolutePath}", !targetFile.isDirectory)
        
        path
    }

    // Lifecycle methods
    override def beforeAll(configMap: ConfigMap): Unit = {
        super.beforeAll(configMap)
        csvFileName foreach { filename =>
            csvFile = new BufferedWriter(new FileWriter(filename))
            csvFile.write("File,Outputs,Mean [ms],StdDev [ms],RelStdDev [%],Best [ms],Median [ms],Worst [ms]")
            csvFile.newLine()
            csvFile.flush()
        }
        inclusionFileName foreach { filename =>
            val source = scala.io.Source.fromFile(filename)
            try {
                testsToInclude = Some(source.getLines().toSet)
            } finally {
                source.close()
            }
        }
    }

    override def afterAll(configMap: ConfigMap): Unit = {
        super.afterAll(configMap)
        if (csvFileName.isDefined) csvFile.close()
    }

    // System under test
    override def systemsUnderTest: Seq[SystemUnderTest] = Vector(testingInstance)

    private val testingInstance: SystemUnderTest with TimingUtils = new SystemUnderTest with TimingUtils {
        lazy val projectInfo: ProjectInfo = ConfigurableCarbonTests.this.projectInfo.update(name)

        override def run(input: AnnotatedTestInput): Seq[AbstractOutput] = {
            testsToInclude foreach { tests =>
                if (!tests.contains(input.name)) {
                    alert(s"Skipping ${input.name} (not included)")
                    return Seq.empty
                }
            }

            val phaseNames: Seq[String] = frontend(verifier, input.files).phases.map(_.name) :+ "Overall"
            val isWarmup = warmupDirName.isDefined && Paths.isInSubDirectory(Paths.canonize(warmupDirName.get), input.file.toFile)
            val reps = if (isWarmup) 1 else repetitions

            // collect data
            val data = for (_ <- 1 to reps) yield {
                val fe = frontend(verifier, input.files)
                val perPhaseTimings = fe.phases.map(p => time(p.f)._2)
                val actualErrors: Seq[AbstractError] =
                    fe.result match {
                        case Success => Nil
                        case Failure(es) => es collect {
                            case e: AbstractVerificationError => e.transformedError()
                            case rest: AbstractError => rest
                        }
                    }
                (actualErrors, perPhaseTimings)
            }

            val (verResults, timeResults) = data.unzip
            if (1 < verResults.length) {
                Predef.assert(verResults.tail.forall(_ == verResults.head), 
                                            s"Did not get the same errors for all repetitions: $verResults")
            }

            val timingsWithTotal = timeResults.toVector.map(row => row :+ row.sum)
            val sortedTimings = timingsWithTotal.sortBy(_.last)
            val (trimmedTimings, isTrimmed) = if (reps >= 4) {
                (sortedTimings.slice(1, reps-1), true)
            } else {
                (sortedTimings, false)
            }
            val meaningfulReps = trimmedTimings.length
            Predef.assert(trimmedTimings.nonEmpty)

            val meanTimings = trimmedTimings.transpose.map(col => (col.sum.toFloat / col.length).toLong)
            val bestRun = trimmedTimings.head
            val medianRun = trimmedTimings(meaningfulReps / 2)
            val worstRun = trimmedTimings.last

            val stddevTimings = trimmedTimings.transpose.zip(meanTimings).map { case (col, mean) =>
                math.sqrt(col.map(v => math.pow((v - mean).toDouble, 2)).sum / col.length).toLong
            }

            val relStddevTimings = stddevTimings.zip(meanTimings).map { case (stddev, mean) =>
                (100.0 * stddev / math.abs(mean)).toLong
            }

            if (isWarmup) {
                info(s"[JVM warmup] Time required: ${printableTimings(meanTimings, phaseNames)}.")
            } else if (meaningfulReps == 1) {
                info(s"[Benchmark] Time required: ${printableTimings(meanTimings, phaseNames)}.")
                if (csvFileName.isDefined) {
                    val csvRowData = Seq(
                        JPaths.get(targetDirName).toAbsolutePath.relativize(input.file.toAbsolutePath),
                        verResults.head.length,
                        meanTimings.last, stddevTimings.last, relStddevTimings.last,
                        bestRun.last, medianRun.last, worstRun.last)
                    csvFile.write(csvRowData.mkString(","))
                    csvFile.newLine()
                    csvFile.flush()
                }
            } else {
                if (isTrimmed) {
                    info(s"[Benchmark] Trimmed ${sortedTimings.length - trimmedTimings.length} marginal runs.")
                }
                val n = "%2d".format(meaningfulReps)
                info(s"[Benchmark] Mean / $n runs: ${printableTimings(meanTimings, phaseNames)}.")
                info(s"[Benchmark] Stddevs:        ${printableTimings(stddevTimings, phaseNames)}.")
                info(s"[Benchmark] Rel. stddev:    ${printablePercentage(relStddevTimings, phaseNames)}.")
                info(s"[Benchmark] Best run:       ${printableTimings(bestRun, phaseNames)}.")
                info(s"[Benchmark] Median run:     ${printableTimings(medianRun, phaseNames)}.")
                info(s"[Benchmark] Worst run:      ${printableTimings(worstRun, phaseNames)}.")

                if (csvFileName.isDefined) {
                    val csvRowData = Seq(
                        JPaths.get(targetDirName).toAbsolutePath.relativize(input.file.toAbsolutePath),
                        verResults.head.length,
                        meanTimings.last, stddevTimings.last, relStddevTimings.last,
                        bestRun.last, medianRun.last, worstRun.last)
                    csvFile.write(csvRowData.mkString(","))
                    csvFile.newLine()
                    csvFile.flush()
                }
            }

            verResults.flatten.map(SilOutput)
        }

        private def printableTimings(timings: Seq[Long], phaseNames: Seq[String]): String =
            timings.map(formatTimeForTable).zip(phaseNames).map(tup => tup._1 + " (" + tup._2 + ")").mkString(", ")

        private def printablePercentage(percentage: Seq[Long], phaseNames: Seq[String]): String = {
            percentage
                .map(p => "%6s %%   ".format(p))
                .zip(phaseNames)
                .map(tup => tup._1 + " (" + tup._2 + ")")
                .mkString(", ")
        }
    }

    // Frontend implementation
    override def frontend(verifier: Verifier, files: Seq[Path]): Frontend = {
        require(files.length == 1, "tests should consist of exactly one file")
        val fe = new CarbonFrontend(NoopReporter, SilentLogger().get)
        fe.init(verifier)
        fe.reset(files.head)
        fe
    }

    // Helper methods
    private def failIf(message: => String, condition: Boolean): Unit =
        if (condition) fail(message)

    private def getConfigStringOption(key: String): Option[String] = {
        val value = config.getOrElse(key, "").toString.trim
        if (value.isEmpty) None else Some(value)
    }
}