package ru.spbau.sazanovich.nikita.calculator.error

/** Used by parts of the Calculator to report errors met during execution. */
trait ErrorReporter {

  def reportError(errorMessage: String): Unit

  def hasErrorOccurred: Boolean
}
