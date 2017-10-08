package ru.spbau.sazanovich.nikita.collect.mutable

import com.google.common.annotations.VisibleForTesting

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Mutable version of MultiSet. */
class MultiSet[A] private (
    private val elementCountMap: mutable.HashMap[A, Int] = mutable.HashMap[A, Int]()) {

  /** Returns whether there is at least one such element. */
  def apply(elem: A): Boolean = elementCountMap.contains(elem)

  /** Adds an element. */
  def add(elem: A): Int = addCount(elem)

  private def addCount(elem: A, count: Int = 1): Int = {
    val equalElementsCount = getCount(elem) + count
    elementCountMap.update(elem, equalElementsCount)
    equalElementsCount
  }

  /** Finds an element. */
  def get(elem: A): Option[A] = Some(elem).filter(apply)

  @VisibleForTesting
  private[mutable] def getCount(elem: A): Int = elementCountMap.getOrElse(elem, 0)

  def filter(p: (A) => Boolean): MultiSet[A] = {
    createNewSetByProcessingElementsWith { (element, count, filteredMultiSet) =>
      if (p(element)) filteredMultiSet.addCount(element, count)
    }
  }

  def withFilter(p: (A) => Boolean): MultiSet[A] = filter(p)

  def map[B](f: (A) => B): MultiSet[B] = {
    createNewSetByProcessingElementsWith { (element, count, mappedMultiSet) =>
      val mappedElement = f(element)
      mappedMultiSet.addCount(mappedElement, count)
    }
  }

  def flatMap[B](f: (A) => Iterable[B]): MultiSet[B] = {
    createNewSetByProcessingElementsWith { (element, count, mappedMultiSet) =>
      val mappedIterable = f(element)
      mappedIterable.foreach(mappedElement => mappedMultiSet.addCount(mappedElement, count))
    }
  }

  /** Creates a new [[MultiSet]] with the elements' count equal to the minimum of sets' count. */
  def &(that: MultiSet[A]): MultiSet[A] = {
    // Because we build an intersection, it is enough to iterate over either of two sets.
    createNewSetByProcessingElementsWith { (element, count, multiSetIntersection) =>
      val countInThatSet = that.getCount(element)
      val intersectionCount = Math.min(count, countInThatSet)
      if (intersectionCount > 0) {
        multiSetIntersection.addCount(element, intersectionCount)
      }
    }
  }

  /** Creates a new [[MultiSet]] with the elements' count equal to the sum of sets' count. */
  def |(that: MultiSet[A]): MultiSet[A] = {
    val multiSetUnion = new MultiSet[A](elementCountMap)
    for ((element, count) <- that.elementCountMap) {
      multiSetUnion.addCount(element, count)
    }
    multiSetUnion
  }

  /** The other entity is equal if it is MultiSet and it has the same elements' count in it. */
  override def equals(that: scala.Any): Boolean = {
    that match {
      case that: MultiSet[A] => elementCountMap == that.elementCountMap
      case _ => false
    }
  }

  /**
    * Process the elements of the MultiSet with with given function
    * @param processor the function which gets current element, its count and the new [[MultiSet]]
    *                  and should update the set in an appropriate way
    */
  private def createNewSetByProcessingElementsWith[B](
      processor: (A, Int, MultiSet[B]) => Unit): MultiSet[B] = {
    val processedSet = new MultiSet[B]()
    for ((element, count) <- elementCountMap) {
      processor(element, count, processedSet)
    }
    processedSet
  }
}

object MultiSet {

  def apply[A](elements: A*): MultiSet[A] = {
    val resultSet = new MultiSet[A]()
    for (element <- elements) {
      resultSet.addCount(element)
    }
    resultSet
  }

  def unapplySeq[A](multiSet: MultiSet[A]): Option[Seq[A]] = {
    val elements = mutable.ArrayBuffer[A]()
    for ((element, count) <- multiSet.elementCountMap) {
      elements ++= List.fill(count)(element)
    }
    Some(elements)
  }
}
