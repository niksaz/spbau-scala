package ru.spbau.sazanovich.nikita.collect.immutable

import scala.collection.mutable

/** Immutable covariant version of MultiSet. */
class MultiSet[+A] private (private val elementCountList: List[(A, Int)]) {

  import MultiSet.mergeInElementCountMap

  /** Used for building up the elementCountMap internally. */
  private def this(elementCountMap: mutable.HashMap[A, Int]) = {
    this(elementCountMap.toList)
  }

  /** Returns whether there is at least one such element. */
  def apply[B >: A](elem: B): Boolean = {
    elementCountList.exists {
      case (e, _) => e == elem
    }
  }

  /** Returns the count of the element. */
  def getCount[B >: A](elem: B): Int = {
    elementCountList.foldLeft(0) { case (foundEqualCount, (element, count)) =>
      if (element == elem) foundEqualCount + count else foundEqualCount
    }
  }

  def filter(p: (A) => Boolean): MultiSet[A] = {
    val filteredElementCountList = elementCountList.filter { case (element, _) => p(element) }
    new MultiSet[A](filteredElementCountList)
  }

  def withFilter(p: (A) => Boolean): MultiSet[A] = filter(p)

  def map[B](f: (A) => B): MultiSet[B] = {
    createNewSetByProcessingElementsWith[B] { (element, count, mappedElementCount) =>
      val mappedElement = f(element)
      mergeInElementCountMap(mappedElement, count, mappedElementCount)
    }
  }

  def flatMap[B](f: (A) => Iterable[B]): MultiSet[B] = {
    createNewSetByProcessingElementsWith[B] { (element, count, mappedElementCount) =>
      val mappedIterable = f(element)
      mappedIterable.foreach { mappedElement =>
        mergeInElementCountMap(mappedElement, count, mappedElementCount)
      }
    }
  }

  /** Creates a new [[MultiSet]] with the elements' count equal to the minimum of sets' count. */
  def &[B >: A](that: MultiSet[B]): MultiSet[B] = {
    val downcastedThis = downcast[B]()
    // Because we build an intersection, it is enough to iterate over either of two sets.
    downcastedThis.createNewSetByProcessingElementsWith[B] {
      (element, count, intersectionElementCount) =>
        val countInThatSet = that.getCount(element)
        val intersectionCount = Math.min(count, countInThatSet)
        if (intersectionCount > 0) {
          mergeInElementCountMap(element, intersectionCount, intersectionElementCount)
        }
    }
  }

  /** Creates a new [[MultiSet]] with the elements' count equal to the sum of sets' count. */
  def |[B >: A](that: MultiSet[B]): MultiSet[B] = {
    val downcastedThis = downcast[B]()
    val unionElementCount = mutable.HashMap[B, Int]()
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
        val downcastedThis = downcast()
        val downcastedThat = that.downcast()
        // Since the list is unsorted, we need to convert it to set, before checking equality.
        downcastedThis.elementCountList.toSet == downcastedThat.elementCountList.toSet
      case _ => false
    }
  }

  /** Since the downcast may occur without our awareness, we need to normalize it each time. */
  private def downcast[B >: A](): MultiSet[B] = map[B](x => x)

  /**
    * Process the elements of the MultiSet with with given function and returns that new
    * [[MultiSet]].
    *
    * @param processor the function which gets current element, its count and the new [[MultiSet]]
    *                  and should update the set in an appropriate way
    */
  private def createNewSetByProcessingElementsWith[B](
      processor: (A, Int, mutable.HashMap[B, Int]) => Unit): MultiSet[B] = {
    val processedElementMap = mutable.HashMap[B, Int]()
    for ((element, count) <- elementCountList) {
      processor(element, count, processedElementMap)
    }
    new MultiSet[B](processedElementMap)
  }
}

object MultiSet {

  def apply[A](elements: A*): MultiSet[A] = {
    val elementCountMap = mutable.HashMap[A, Int]()
    for (element <- elements) {
      mergeInElementCountMap(element, 1, elementCountMap)
    }
    new MultiSet[A](elementCountMap)
  }

  def unapplySeq[A](multiSet: MultiSet[A]): Option[Seq[A]] = {
    val elements = mutable.ArrayBuffer[A]()
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
