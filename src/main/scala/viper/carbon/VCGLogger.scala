package viper.carbon

import viper.silver.ast.Program

trait VCGLogger {
  def onStartVerification(program: Program): Unit = {}
  def onTranslationCompleted(stats: Map[String, Int]): Unit = {}
  def onInvokeBoogie(options: Seq[String]): Unit = {}
  def onBoogieStatistics(instance: Int, stats: Map[String, Double]): Unit = {}
  def onInstanceSuccess(instance: Int): Unit = {}
  def onInstanceStart(instance: Int): Unit = {}
  def onStop(): Unit = {}
}

object VCGLogger {
  object Noop extends VCGLogger
}
