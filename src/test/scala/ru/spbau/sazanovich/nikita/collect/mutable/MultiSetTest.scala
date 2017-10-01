package ru.spbau.sazanovich.nikita.collect.mutable

import org.scalatest.FunSuite

class MultiSetTest extends FunSuite {

  test("addNewElement") {
    val multiSet = MultiSet(1, 2, 3, 1)
    multiSet.add(5)
    assert(multiSet(5))
    assert(multiSet.getCount(5) == 1)
  }

  test("addExistingElement") {
    val multiSet = MultiSet(1, 2, 3)
    multiSet.add(2)
    assert(multiSet(2))
    assert(multiSet.getCount(2) == 2)
  }

  test("findExistingElement") {
    val multiSet = MultiSet("hello", "hi")
    assert(multiSet.get("hello") == Option("hello"))
    assert(multiSet.get("hi") == Option("hi"))
  }

  test("findNonExistentElement") {
    val multiSet = MultiSet("hello", "hi")
    assert(multiSet.get("hola") == Option.empty)
    assert(multiSet.get("bonjour") == Option.empty)
  }

  test("filter") {
    val multiSet = MultiSet(1, 2, 3, 4, 4, 5)
    val filteredMultiSet = multiSet.filter(x => x % 2 == 0)
    assert(filteredMultiSet == MultiSet(2, 4, 4))
  }

  test("map") {
    val multiSet = MultiSet(1, 2, 3, 4, 4, 5)
    val mappedMultiSet = multiSet.map(x => x * 2)
    assert(mappedMultiSet == MultiSet(2, 4, 6, 8, 8, 10))
  }

  test("flatMap") {
    val multiSet = MultiSet("hello", "world")
    val mappedMultiSet = multiSet.flatMap(s => s.chars().toArray)
    val expectedMultiSet = MultiSet('h', 'e', 'l', 'l', 'o', 'w', 'o', 'r', 'l', 'd')
    assert(mappedMultiSet == expectedMultiSet)
  }

  test("forComprehensionMap") {
    val multiSet = MultiSet(1, 3, 2, 1)
    val mappedMultiSet =
      for {
        e <- multiSet
      } yield e * 2
    assert(mappedMultiSet == MultiSet(2, 6, 4, 2))
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
    assert(mappedMultiSet == expectedMultiSet)
  }

  test("forComprehensionFilter") {
    val multiSet = MultiSet(2, 5, 9, 10)
    val filteredMultiSet =
      for {
        e <- multiSet
        if e % 2 == 0
      } yield e
    assert(filteredMultiSet == MultiSet(2, 10))
  }

  test("multiSetIntersection") {
    val multiSetA = MultiSet(1, 2, 3, 4, 4)
    val multiSetB = MultiSet(3, 4, 5, 5)
    val multiSetIntersection = multiSetA & multiSetB
    assert(multiSetIntersection == MultiSet(3, 4))
  }

  test("multiSetUnion") {
    val multiSetA = MultiSet(1, 2, 3, 4, 4)
    val multiSetB = MultiSet(3, 4, 5, 5)
    val multiSetUnion = multiSetA | multiSetB
    assert(multiSetUnion == MultiSet(1, 2, 3, 4, 4, 3, 4, 5, 5))
  }

  test("patternMatching") {
    val multiSet = MultiSet(1, 2, 3, 3)
    multiSet match {
        // Since the unapply method does not impose any order guarantees.
      case MultiSet(a, b, c, d) => assert(MultiSet(a, b, c, d) == multiSet)
      case _ => fail("Not matched correctly!")
    }
  }
}
