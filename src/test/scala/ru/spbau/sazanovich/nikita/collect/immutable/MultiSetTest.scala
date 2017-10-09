package ru.spbau.sazanovich.nikita.collect.immutable

import org.scalatest.FunSuite
import com.google.common.truth.Truth.assertThat

class MultiSetTest extends FunSuite {

  import MultiSetTest._

  test("findExistingElement") {
    val multiSet = MultiSet("hello", "hi")
    assert(multiSet("hello"))
    assert(multiSet("hi"))
  }

  test("findNonExistentElement") {
    val multiSet = MultiSet("hello", "hi")
    assert(!multiSet("hola"))
    assert(!multiSet("bonjour"))
  }

  test("filter") {
    val multiSet = MultiSet(1, 2, 3, 4, 4, 5)
    val filteredMultiSet = multiSet.filter(_ % 2 == 0)
    assertThat(filteredMultiSet).isEqualTo(MultiSet(2, 4, 4))
  }

  test("map") {
    val multiSet = MultiSet(1, 2, 3, 4, 4, 5)
    val mappedMultiSet = multiSet.map(_ * 2)
    assertThat(mappedMultiSet).isEqualTo(MultiSet(2, 4, 6, 8, 8, 10))
  }

  test("flatMap") {
    val multiSet = MultiSet("hello", "world")
    val mappedMultiSet = multiSet.flatMap(_.chars().toArray)
    val expectedMultiSet = MultiSet('h', 'e', 'l', 'l', 'o', 'w', 'o', 'r', 'l', 'd')
    assertThat(mappedMultiSet).isEqualTo(expectedMultiSet)
  }

  test("forComprehensionMap") {
    val multiSet = MultiSet(1, 3, 2, 1)
    val mappedMultiSet =
      for {
        e <- multiSet
      } yield e * 2
    assertThat(mappedMultiSet).isEqualTo(MultiSet(2, 6, 4, 2))
  }

  test("forComprehensionFlatMap") {
    val outerMultiSet = MultiSet(1, 2, 3)
    val innerMultiSet = List(2, 3, 4)
    val mappedMultiSet =
      for {
        outer <- outerMultiSet
        inner <- innerMultiSet
      } yield outer * inner
    val expectedMultiSet = MultiSet(2, 3, 4, 4, 6, 8, 6, 9, 12)
    assertThat(mappedMultiSet).isEqualTo(expectedMultiSet)
  }

  test("forComprehensionFilter") {
    val multiSet = MultiSet(2, 5, 9, 10)
    val filteredMultiSet =
      for {
        e <- multiSet
        if e % 2 == 0
      } yield e
    assertThat(filteredMultiSet).isEqualTo(MultiSet(2, 10))
  }

  test("multiSetIntersection") {
    val multiSetA = MultiSet(1, 2, 3, 4, 4)
    val multiSetB = MultiSet(3, 4, 5, 5)
    val multiSetIntersection = multiSetA & multiSetB
    assertThat(multiSetIntersection).isEqualTo(MultiSet(3, 4))
  }

  test("multiSetUnion") {
    val multiSetA = MultiSet(1, 2, 3, 4, 4)
    val multiSetB = MultiSet(3, 4, 5, 5)
    val multiSetUnion = multiSetA | multiSetB
    assertThat(multiSetUnion).isEqualTo(MultiSet(1, 2, 3, 4, 4, 3, 4, 5, 5))
  }

  test("patternMatching") {
    val multiSet = MultiSet(1, 2, 3, 3)
    multiSet match {
      // Since the unapply method does not impose any order guarantees.
      case MultiSet(a, b, c, d) => assertThat(MultiSet(a, b, c, d)).isEqualTo(multiSet)
      case _ => fail("Not matched correctly!")
    }
  }

  test("covarianceTest") {
    val multiSetChild: MultiSet[TestChild] = MultiSet(TestChild("a", 1), TestChild("a", 2))
    val multiSetParent: MultiSet[TestParent] = multiSetChild
    assert(multiSetParent.getCount(new TestParent("a")) == 2)
  }
}

object MultiSetTest {

  class TestParent(val name: String) {

    override def equals(that: scala.Any): Boolean = {
      that match {
        case that: TestParent => name == that.name
        case _ => false
      }
    }
  }

  case class TestChild(override val name: String, id: Int) extends TestParent(name)
}
