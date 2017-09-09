package ru.spbau.sazanovich.nikita.calculator.error

/** Reports errors to console. */
class ConsoleErrorReporter extends ErrorReporter {

  private var errorOccurred = false

  override def reportError(errorMessage: String): Unit = {
    println("Error: " + errorMessage)
    errorOccurred = true
  }

  override def hasErrorOccurred: Boolean = errorOccurred
}
