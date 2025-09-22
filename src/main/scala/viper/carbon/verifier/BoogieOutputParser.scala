package viper.carbon.verifier

import viper.silver.verifier.errors.Internal
import viper.silver.verifier.reasons.InternalReason
import viper.silver.verifier.DummyNode
import viper.silver.verifier.AbstractError

class BoogieOutputParser(
  logListener: viper.carbon.VCGLogger
) {

  // Number pattern for integers or floats.
  private val NumberRegex = raw"""(\d+\.\d+|\d+)"""

  private var versionFound: String = null
  private var internalErrorCounter = 0
  private val errorIds = collection.mutable.ListBuffer[Int]()
  private var errormap: Map[Int, AbstractError] = Map()
  val models = collection.mutable.ListBuffer[String]()

  private var parsingModel : Option[StringBuilder] = None
  private var stateInitialBlock = false

  // Old style stats block accumulators
  private var inRelevantStatBlock = false
  private val statsMap = scala.collection.mutable.LinkedHashMap[String,Double]()

  // smt.stats header/value support
  private var pendingErrStatKeys: Option[Seq[String]] = None
  private var pendingErrStatValues: Seq[Double] = Seq()
  private var haveReceivedValues: Boolean = false
  private var boogieErrorCounter = 0

  private def unexpected(msg: String): Unit = {
      internalErrorCounter -= 1
      errorIds += internalErrorCounter
      val internalError = Internal(InternalReason(DummyNode, msg))
      errormap += (internalErrorCounter -> internalError)
  }

  def feedLine(line: String): Unit = synchronized {
    val State = """^\*\*\* (.*)$$""".r
    val Logo = """Boogie program verifier version ([0-9.]+),.*""".r
    val Summary = raw"""Boogie program verifier finished with ([0-9]+) verified, ([0-9]+) error.*""".r
    val Error = """  .+\[([0-9]+)\]""".r
    val AnyStat = raw"""^\[SMT(?:-OUT)?(?:-ERR)?-\d+\]\s+.*$$""".r
    val ProverError = raw"""^Prover error:\s*(.+)$$""".r
    println(line)
    
    line match {
      case "" => ()
      case State(msg) => {
        msg match {
          case "END_STATE" => stateInitialBlock = false
          case "<initial>" => stateInitialBlock = true
          case "END_MODEL" if parsingModel.isDefined =>
            models.append(parsingModel.get.toString())
            parsingModel = None
          case "MODEL" if parsingModel.isEmpty => parsingModel = Some(new StringBuilder)
          case unparsable => unexpected(s"Unparsable Boogie state line: $unparsable")
        }
      }
      case _ if stateInitialBlock => 
        ()
      case _ if parsingModel.isDefined => 
        parsingModel.get.append(line).append("\n")
      case Logo(v) => 
        versionFound = v
      case Error(id) => 
        errorIds += id.toInt
        boogieErrorCounter += 1
      case Summary(_, e) =>
        if (e.toInt != boogieErrorCounter) unexpected(raw"Found ${errorIds.size} errors, but there should be $e. Line: $line")
      case AnyStat() => parseStatOrError(line)
      case ProverError(_) => 
        () // Some tactic failed, probably a non-issue
      case unparsable => 
        unexpected(s"Unparsable Boogie output: $unparsable")
    }
  }

  private def parseStatOrError(line: String): Unit = {
    val Err = raw"""^\[SMT(?:-ERR)-(\d+)\]\s+(.*)$$""".r
    val Out = raw"""^\[SMT(?:-OUT)?-(\d+)\]\s+(.*)$$""".r
    val Starting = raw"""^\[SMT-(\d+)\] Starting.*$$""".r

    line match {
      case Starting(instance) => 
        logListener.onInstanceStart(instance.toInt)
        pendingErrStatKeys = None
        haveReceivedValues = false
      case Err(instance, content) => 
        val ErrStatsHeader = raw"""\(smt.stats((?:\s+:\S+)+)\s*\)$$""".r
        val ErrStatsValue = raw"""\(smt.stats((?:\s+\S+)+)\s*\)$$""".r
        content match {
          case ErrStatsHeader(keys) =>
            val keySeq = keys.trim.split(" +").toSeq
            if (haveReceivedValues) {
              // overwrite, since header new header start
              pendingErrStatKeys = Some(keySeq)
              haveReceivedValues = false
            }  else {
              // append, since header continuation
              pendingErrStatKeys match {
                case None => pendingErrStatKeys = Some(keySeq)
                case Some(existing) => pendingErrStatKeys = Some(existing ++ keySeq)
              }
            }
          case ErrStatsValue(values) =>
            if (pendingErrStatKeys.isEmpty) {
              unexpected(s"Got smt.stats values line without preceding header: $line")
            } else {
              haveReceivedValues = true
              val Number = NumberRegex.r
              val TwoNumbers = raw"""$NumberRegex/$NumberRegex""".r
              val valsSeq = values.trim.split(" +").map {
                case Number(value) => value.toDouble
                case TwoNumbers(v1, _) => v1.toDouble // For now just ignore the second number
              }
              val keysSeq = pendingErrStatKeys.get
              pendingErrStatValues = pendingErrStatValues ++ valsSeq
              if (pendingErrStatValues.size == keysSeq.size) {
                val stats = (keysSeq zip pendingErrStatValues).toMap
                logListener.onBoogieStatistics(instance.toInt, stats)
                pendingErrStatValues = Seq()
              } else if (pendingErrStatValues.size > keysSeq.size) {
                unexpected(s"Got more smt.stats values (${pendingErrStatValues.size}) than keys (${keysSeq.size}): $line")
              } // else wait for more values
            }
          case _ =>
            () // ignore errors we don't care about
        }
      case Out(instance, content) => 
        val SingleLine = raw"""\((:[^\s\)]+) $NumberRegex\)$$""".r
        val LineContent = raw""":(\S+)\s+$NumberRegex"""
        val StartLine = raw"""\(:added-eqs\s+$NumberRegex$$""".r
        val EndLine = raw"""$LineContent\)$$""".r
        val ContinueLine = raw"""$LineContent$$""".r
        content match {
          case SingleLine(name, value) =>
            logListener.onBoogieStatistics(instance.toInt, Map(name -> value.toDouble))
          case "unsat" =>
            logListener.onInstanceSuccess(instance.toInt)
          case StartLine(value) if !inRelevantStatBlock =>
            statsMap += ("added-eqs" -> value.toDouble)
            inRelevantStatBlock = true
          case StartLine(_) if inRelevantStatBlock =>
            unexpected(raw"Starting stat z3 block while in z3 stat block: $line")
          case ContinueLine(name, value) if inRelevantStatBlock =>
            statsMap += (name -> value.toDouble)
          case EndLine(name, value) if inRelevantStatBlock =>
            statsMap += (name -> value.toDouble)
            logListener.onBoogieStatistics(instance.toInt, statsMap.toMap)
            statsMap.clear(); inRelevantStatBlock = false
          case _ =>
            () // ignore statistics we don't care about
        }
      case unparsable => 
        unexpected(s"Unparsable Boogie statistic: $unparsable")
    }
  }

  def getResult(): (String, Seq[Int], Map[Int, AbstractError], Seq[String]) = synchronized {
    (versionFound, errorIds.toSeq, errormap, models.toSeq)
  }
}