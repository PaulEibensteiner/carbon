package viper.carbon

import viper.silver.ast.Program

trait VCGLogger {
  def onStartVerification(program: Program): Unit = {}
  def onTranslationCompleted(stats: Map[String, Int]): Unit = {}
  def onInvokeBoogie(options: Seq[String]): Unit = {}
  def onBoogieStatistics(stats: Map[String, String]): Unit = {}
  def onStop(): Unit = {}
}

object VCGLogger {
  object Noop extends VCGLogger
}
