package ru.spbau.sazanovich.nikita.collect.mutable

import com.google.common.annotations.VisibleForTesting

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Mutable version of MultiSet. */
class MultiSet[A] private (
    val elementCountMap: mutable.HashMap[A, Int] = mutable.HashMap.empty[A, Int]) {

  /** Returns whether there is at least one such element. */
  def apply(elem: A): Boolean = {
    elementCountMap.contains(elem)
  }

  /** Adds an element. */
  def add(elem: A): Int = addCount(elem, 1)

  private def addCount(elem: A, count: Int): Int = {
    assert(count > 0)
    var equalElementsCount = getCount(elem)
    equalElementsCount += count
    elementCountMap.update(elem, equalElementsCount)
    equalElementsCount
  }

  /** Finds an element. */
  def get(elem: A): Option[A] = {
    if (getCount(elem) > 0) Option.apply(elem) else Option.empty
  }

  @VisibleForTesting
  private[mutable] def getCount(elem: A): Int = {
    elementCountMap.getOrElse(elem, 0)
  }

  def filter(p: (A) => Boolean): MultiSet[A] = {
    val filteredMultiSet = new MultiSet[A]()
    for ((element, count) <- elementCountMap) {
      if (p(element)) {
        filteredMultiSet.addCount(element, count)
      }
    }
    filteredMultiSet
  }

  def withFilter(p: (A) => Boolean): MultiSet[A] = filter(p)

  def map[B](f: (A) => B): MultiSet[B] = {
    val mappedMultiSet = new MultiSet[B]()
    for ((element, count) <- elementCountMap) {
      val mappedElement = f(element)
      mappedMultiSet.addCount(mappedElement, count)
    }
    mappedMultiSet
  }

  def flatMap[B](f: (A) => Iterable[B]): MultiSet[B] = {
    val mappedMultiSet = new MultiSet[B]()
    for ((element, count) <- elementCountMap) {
      val mappedIterable = f(element)
      mappedIterable.foreach(mappedElement => mappedMultiSet.addCount(mappedElement, count))
    }
    mappedMultiSet
  }

  /** Creates a new [[MultiSet]] with the elements' count equal to the minimum of sets' count. */
  def &(that: MultiSet[A]): MultiSet[A] = {
    val multiSetIntersection = new MultiSet[A]()
    // Because we build an intersection, it is enough to iterate over either of two sets.
    for ((element, count) <- elementCountMap) {
      val countInThatSet = that.getCount(element)
      val intersectionCount = Math.min(count, countInThatSet)
      if (intersectionCount > 0) {
        multiSetIntersection.addCount(element, intersectionCount)
      }
    }
    multiSetIntersection
  }

  /** Creates a new [[MultiSet]] with the elements' count equal to the sum of sets' count. */
  def |(that: MultiSet[A]): MultiSet[A] = {
    val multiSetUnion = new MultiSet[A]()
    for ((element, count) <- this.elementCountMap) {
      multiSetUnion.addCount(element, count)
    }
    for ((element, count) <- that.elementCountMap) {
      multiSetUnion.addCount(element, count)
    }
    multiSetUnion
  }

  /** The other entity is equal if it is MultiSet and it has the same elements' count in it. */
  override def equals(that: scala.Any): Boolean = {
    that match {
      case that: MultiSet[A] => this.elementCountMap == that.elementCountMap
      case _ => false
    }
  }
}

object MultiSet {

  def apply[A](elements: A*): MultiSet[A] = {
    val resultSet = new MultiSet[A]()
    for (element <- elements) {
      resultSet.addCount(element, 1)
    }
    resultSet
  }

  def unapplySeq[A](multiSet: MultiSet[A]): Option[Seq[A]] = {
    val elements = ArrayBuffer.empty[A]
    for ((element, count) <- multiSet.elementCountMap) {
      elements ++= List.fill(count)(element)
    }
    Some(elements)
  }
}
