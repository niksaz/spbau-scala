package ru.spbau.sazanovich.nikita.collect.immutable

import com.google.common.annotations.VisibleForTesting

import scala.collection.immutable
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Immutable covariant version of MultiSet. */
class MultiSet[+A] private (val elementCountList: immutable.List[(A, Int)]) {

  import MultiSet.mergeInElementCountMap

  /** Used for building up the elementCountMap internally. */
  private def this(elementCountMap: mutable.HashMap[A, Int]) = {
    this(immutable.List(elementCountMap.toSeq:_*))
  }

  /** Returns whether there is at least one such element. */
  def apply[B >: A](elem: B): Boolean = {
    elementCountList.iterator.exists(e => e._1 == elem)
  }

  /** Finds an element. */
  def get[B >: A](elem: B): Option[B] = {
    elementCountList.iterator.find(e => e._1 == elem).map(e => e._1)
  }

  @VisibleForTesting
  private[immutable] def getCount[B >: A](elem: B): Int = {
    var elemCount = 0
    elementCountList.iterator.foreach(e => {
      if (e._1 == elem) {
        elemCount += e._2
      }
    })
    elemCount
  }

  def filter(p: (A) => Boolean): MultiSet[A] = {
    new MultiSet[A](elementCountList.filter(e => p(e._1)))
  }

  def withFilter(p: (A) => Boolean): MultiSet[A] = filter(p)

  def map[B](f: (A) => B): MultiSet[B] = {
    val mappedElementCount = mutable.HashMap.empty[B, Int]
    for ((element, count) <- elementCountList) {
      val mappedElement = f(element)
      mergeInElementCountMap(mappedElement, count, mappedElementCount)
    }
    new MultiSet[B](mappedElementCount)
  }

  def flatMap[B](f: (A) => Iterable[B]): MultiSet[B] = {
    val mergedElementCountMap = mutable.HashMap.empty[B, Int]
    for ((element, count) <- elementCountList) {
      val mappedIterable = f(element)
      mappedIterable.foreach(mappedElement => {
        mergeInElementCountMap(mappedElement, count, mergedElementCountMap)
      })
    }
    new MultiSet[B](mergedElementCountMap)
  }

  /** Creates a new [[MultiSet]] with the elements' count equal to the minimum of sets' count. */
  def &[B >: A](that: MultiSet[B]): MultiSet[B] = {
    val downcastedThis = downcast[B]()
    val intersectionElementCount = mutable.HashMap.empty[B, Int]
    // Because we build an intersection, it is enough to iterate over either of two sets.
    for ((element, count) <- downcastedThis.elementCountList) {
      val countInThatSet = that.getCount(element)
      val intersectionCount = Math.min(count, countInThatSet)
      if (intersectionCount > 0) {
        mergeInElementCountMap(element, intersectionCount, intersectionElementCount)
      }
    }
    new MultiSet[B](intersectionElementCount)
  }

  /** Creates a new [[MultiSet]] with the elements' count equal to the sum of sets' count. */
  def |[B >: A](that: MultiSet[B]): MultiSet[B] = {
    val downcastedThis = this.downcast[B]()
    val unionElementCount = mutable.HashMap.empty[B, Int]
    for ((element, count) <- downcastedThis.elementCountList) {
      mergeInElementCountMap(element, count, unionElementCount)
    }
    for ((element, count) <- that.elementCountList) {
      mergeInElementCountMap(element, count, unionElementCount)
    }
    new MultiSet[B](unionElementCount)
  }

  /** The other entity is equal if it is MultiSet and it has the same elements' count in it. */
  override def equals(that: scala.Any): Boolean = {
    that match {
      case that: MultiSet[A] =>
        val downcastedThis = this.downcast()
        val downcastedThat = that.downcast()
        // Since the list is unsorted, we need to convert it to set, before checking equality.
        downcastedThis.elementCountList.toSet == downcastedThat.elementCountList.toSet
      case _ => false
    }
  }

  /** Since the downcast may occur without out understanding, we need to normalize it each time. */
  private def downcast[B >: A](): MultiSet[B] = {
    val elementCountDowncasted = mutable.HashMap.empty[B, Int]
    for ((element, count) <- this.elementCountList) {
      mergeInElementCountMap(element, count, elementCountDowncasted)
    }
    new MultiSet[B](elementCountDowncasted)
  }
}

object MultiSet {

  def apply[A](elements: A*): MultiSet[A] = {
    val elementCountMap = mutable.HashMap.empty[A, Int]
    for (element <- elements) {
      mergeInElementCountMap(element, 1, elementCountMap)
    }
    new MultiSet[A](elementCountMap)
  }

  def unapplySeq[A](multiSet: MultiSet[A]): Option[Seq[A]] = {
    val elements = ArrayBuffer.empty[A]
    for ((element, count) <- multiSet.elementCountList) {
      elements ++= List.fill(count)(element)
    }
    Some(elements)
  }

  private def mergeInElementCountMap[A](
      elem: A, count: Int, elementCountMap: mutable.HashMap[A, Int]): Unit = {
    var equalElementsCount = elementCountMap.getOrElse(elem, 0)
    equalElementsCount += count
    elementCountMap.update(elem, equalElementsCount)
  }
}
