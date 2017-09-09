package ru.spbau.sazanovich.nikita.calculator.error

import org.mockito.Matchers.any
import org.mockito.Mockito.{times, verify}
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{BeforeAndAfter, FunSuite}

/** Creates mock [[ErrorReporter]] before each tests. */
trait TestSuiteWithErrorReporter extends FunSuite with BeforeAndAfter with MockitoSugar {

  var errorReporter: ErrorReporter = _

  before {
    errorReporter = mock[ErrorReporter]
  }

  def verifyNumberOfErrorsReported(expectedNumberOfErrorsReported: Int): Unit = {
    verify(errorReporter, times(expectedNumberOfErrorsReported)).reportError(any())
  }
}
