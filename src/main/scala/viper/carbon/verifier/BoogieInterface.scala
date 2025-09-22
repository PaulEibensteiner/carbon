// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2021 ETH Zurich.

package viper.carbon.verifier

import viper.carbon.boogie.{Assert, Program}
import viper.silver.reporter.BackendSubProcessStages._
import viper.silver.reporter.{BackendSubProcessReport, Reporter}
import viper.silver.testing.BenchmarkStatCollector
import viper.silver.verifier._

import java.io._
import scala.jdk.CollectionConverters._
import scala.util.Random
import viper.carbon.CarbonVerifier

class BoogieDependency(_location: String) extends Dependency {
  def name = "Boogie"
  def location = _location
  var version = "" // filled-in when Boogie is invoked
}

class InputStreamConsumer(val is: InputStream, actionBeforeConsumption: () => Unit) extends Runnable {
  var result : Option[String] = None

  private def convertStreamToString(is: InputStream) = {
    val s = new java.util.Scanner(is).useDelimiter("\\A")
    if (s.hasNext) s.next() else ""
  }

  def run(): Unit = {
    actionBeforeConsumption()
    result = Some(convertStreamToString(is))
    is.close()
  }
}

case class FailureContextImpl(counterExample: Option[Counterexample]) extends FailureContext

/**
  * Defines a clean interface to invoke Boogie and get a list of errors back.
  */

trait BoogieInterface { this: CarbonVerifier =>

  def reporter: Reporter

  def defaultOptions = Seq("/vcsCores:" + java.lang.Runtime.getRuntime.availableProcessors,
    "/proverOpt:VERBOSITY=1", // pass z3 output to boogie stdout
    "/proverOpt:O:verbose=3", // Make z3 output stat info
    "/errorTrace:0",
    "/errorLimit:10000000",
    "/proverOpt:O:smt.AUTO_CONFIG=false",
    "/proverOpt:O:smt.CASE_SPLIT=3",
    "/proverOpt:O:smt.DELAY_UNITS=true",
    "/proverOpt:O:smt.MBQI=false",
    "/proverOpt:O:smt.QI.EAGER_THRESHOLD=100",
    "/proverOpt:O:pp.BV_LITERALS=false",  // added
    "/proverOpt:O:smt.arith.solver=2",
    "/proverOpt:O:smt.qi.max_multi_patterns=1000",
    s"/proverOpt:PROVER_PATH=$z3Path")

  def randomOptions(seed: Int): Seq[String] = Seq(
    s"/proverOpt:O:sat.random_seed=$seed",
    s"/proverOpt:O:nlsat.seed=$seed",
    s"/proverOpt:O:fp.spacer.random_seed=$seed",
    s"/proverOpt:O:smt.random_seed=$seed",
    s"/proverOpt:O:sls.random_seed=$seed")

  val timeoutErrorName = "TIMEOUT"

  /** The (resolved) path where Boogie is supposed to be located. */
  def boogiePath: String

  /** The (resolved) path where Z3 is supposed to be located. */
  def z3Path: String

  private var _boogieProcess: Option[Process] = None
  private var _boogieProcessPid: Option[Long] = None

  // Z3 processes cannot be collected this way, perhaps because they are not created using Java 9 API (but via Boogie).
  // Hence, for now we have to trust Boogie to manage its own sub-processes.
  // private var _z3ProcessStream: Option[LazyList[ProcessHandle]] = None

  var errormap: Map[Int, AbstractError] = Map()

  def invokeBoogie(program: Program, options: Seq[String], timeout: Option[Int], randomize: Boolean, randomSeed: Option[Int]): (String,VerificationResult) = {
    // find all errors and assign everyone a unique id
    errormap = Map()
    program.visit {
      case a@Assert(_, error) =>
        errormap += (a.id -> error)
    }

    var allOptions = defaultOptions ++ options
    if (randomize)
      allOptions = allOptions ++ Seq(s"/randomSeed:${Random.nextInt(10000)}")
    randomSeed match {
      case Some(seed) =>
        allOptions = allOptions ++ randomOptions(seed)
      case _ =>
    }

    // invoke Boogie and parse output in real-time
    val (version, errorIds, models) = run(program.toString, allOptions, timeout)
    // build result
    if (errorIds.isEmpty) {
      (version, Success)
    } else {
      val errors = (0 until errorIds.length).map(i => {
        val id = errorIds(i)
        val error = errormap.get(id).get
        if (models.nonEmpty) {
          error match {
            case e: AbstractVerificationError =>
              e.failureContexts = Seq(FailureContextImpl(Some(SimpleCounterexample(Model(models(i))))))
            case _ =>
          }
        }
        error
      })
      (version, Failure(errors))
    }
  }

  // previous inner parser replaced by external BoogieOutputParser

  private class StreamParsingThread(is: InputStream, parser: BoogieOutputParser, onStart: () => Unit) extends Thread {
    override def run(): Unit = {
      onStart()
      val br = new BufferedReader(new InputStreamReader(is))
      try {
        var line: String = null
        while ({ line = br.readLine(); line != null }) {
          parser.feedLine(line)
        }
      } finally {
        try br.close() catch { case _: Throwable => () }
      }
    }
  }

  /**
    * Invoke Boogie.
    */
  private def run(input: String, options: Seq[String], timeout: Option[Int]) : (String, Seq[Int], Seq[String]) = {
    reporter report BackendSubProcessReport("carbon", boogiePath, BeforeInputSent, _boogieProcessPid)

    val cmd: Seq[String] = Seq(boogiePath) ++ options ++ Seq("stdin.bpl")
    val pb: ProcessBuilder = new ProcessBuilder(cmd.asJava)
    val proc: Process = pb.start()
    _boogieProcess = Some(proc)
    _boogieProcessPid = Some(proc.pid)

    val proverShutdownHook = new Thread {
      override def run(): Unit = {
        destroyProcessAndItsChildren(proc, boogiePath)
      }
    }
    Runtime.getRuntime.addShutdownHook(proverShutdownHook)

    reporter report BackendSubProcessReport("carbon", boogiePath, AfterInputSent, _boogieProcessPid)


    val parser = new BoogieOutputParser(
      logListener
    )

    val stdoutThread =
      new StreamParsingThread(proc.getInputStream, parser, () => reporter report BackendSubProcessReport("carbon", boogiePath, OnOutput, _boogieProcessPid))
    val stderrThread =
      new StreamParsingThread(proc.getErrorStream, parser, () => reporter report BackendSubProcessReport("carbon", boogiePath, OnError, _boogieProcessPid))

    stdoutThread.start()
    stderrThread.start()

    val before = System.currentTimeMillis()
    proc.getOutputStream.write(input.getBytes)
    proc.getOutputStream.close()

    var boogieTimeout = false
    try {
      timeout match {
        case Some(t) if t > 0 =>
          boogieTimeout = !proc.waitFor(t, java.util.concurrent.TimeUnit.SECONDS)
        case _ =>
          proc.waitFor()
      }
    } finally {
      destroyProcessAndItsChildren(proc, boogiePath)
    }
    val after = System.currentTimeMillis()
    BenchmarkStatCollector.addToStat("boogieTime", after - before)

    Runtime.getRuntime.removeShutdownHook(proverShutdownHook)

    stdoutThread.join()
    stderrThread.join()

    reporter report BackendSubProcessReport("carbon", boogiePath, OnExit, _boogieProcessPid)

    val (versionFound, errorIds, internalErrorMap, collectedModels) = parser.getResult()
    errormap = errormap ++ internalErrorMap
    if (boogieTimeout) {
      val id = - (errormap.size + 1)
      val timeoutError = TimeoutOccurred(timeout.get, "second(s)")
      errormap += (id -> timeoutError)
      (versionFound, errorIds :+ id, collectedModels)
    } else {
      (versionFound, errorIds, collectedModels)
    }
  }

  private def destroyProcessAndItsChildren(proc: Process, processPath: String) : Unit = {
    if(proc.isAlive) {
      reporter report BackendSubProcessReport("carbon", processPath, BeforeTermination, _boogieProcessPid)
      proc.children().forEach(_.destroy() : Unit)
      proc.destroy()
      reporter report BackendSubProcessReport("carbon", processPath, AfterTermination, _boogieProcessPid)
    }
  }

  def stopBoogie(): Unit = {
    _boogieProcess match {
      case Some(proc) =>
        destroyProcessAndItsChildren(proc, boogiePath)
      case None =>
    }
  }
}