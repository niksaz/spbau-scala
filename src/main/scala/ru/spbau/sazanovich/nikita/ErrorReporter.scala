package ru.spbau.sazanovich.nikita

/** Used by parts of the Calculator to report errors met during execution. */
trait ErrorReporter {

  def reportError(errorMessage: String): Unit
}
