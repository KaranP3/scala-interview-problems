package com.github.karanp3.lists

import scala.annotation.tailrec

sealed abstract class RList[+T] {
  /**
   * Standard functions
   */
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean
  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)

  /**
   * Easy problems
   */
  // get element at a given index
  def apply(index: Int): T

  // the size of the list
  def length: Int

  // reverse the list
  def reverse: RList[T]

  // concatenating another list
  def ++[S >: T](anotherList: RList[S]): RList[S]

  // remove an element at a given index, return a NEW list
  def removeAt(index: Int): RList[T]
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: RList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def toString: String = "[]"

  /**
   * Easy problems
   */
  // get element at a given index
  override def apply(index: Int): Nothing = throw new NoSuchElementException

  // the size of the list
  override def length: Int = 0;

  // reverse the list
  override def reverse: RList[Nothing] = RNil

  // concatenating another list
  override def ++[S >: Nothing](anotherList: RList[S]): RList[S] = anotherList

  // remove an element at a given index, return a NEW list
  override def removeAt(index: Int): RList[Nothing] = throw new NoSuchElementException
}

case class ::[+T](override val head: T, override val tail: RList[T])
    extends RList[T] {
  override def isEmpty: Boolean = false
  override def toString: String = {
    @tailrec
    def toStringTailRec(remaining: RList[T], result: String): String = {
      if (remaining.isEmpty) result
      else if (remaining.tail.isEmpty) s"$result${remaining.head}"
      else toStringTailRec(remaining.tail, s"$result${remaining.head}, ")
    }

    "[" + toStringTailRec(this, "") + "]"
  }

  /**
   * Easy problems
   */

  // get element at a given index
  override def apply(index: Int): T = {
    @tailrec
    def applyTailRec(remaining: RList[T], currentIndex: Int): T = {
      if (currentIndex == index) remaining.head
      else applyTailRec(remaining.tail, currentIndex + 1)
    }

    if (index < 0) throw new NoSuchElementException
    else applyTailRec(this, 0)
  }

  // the size of the list
  override def length: Int = {
    @tailrec
    def lengthTailRec(list: RList[T], acc: Int): Int = {
      if (list.isEmpty) acc
      else lengthTailRec(list.tail, acc + 1)
    }

    lengthTailRec(this, 0)
  }

  override def reverse: RList[T] = {
    @tailrec
    def reverseTailRec(remainingList: RList[T], reversedList: RList[T]): RList[T] = {
      if (remainingList.isEmpty) reversedList
      else reverseTailRec(remainingList.tail, remainingList.head :: reversedList)
    }

    reverseTailRec(this, RNil)
  }

  override def ++[S >: T](anotherList: RList[S]): RList[S] =  {
   @tailrec
   def concatTailRec(remaining: RList[S], acc: RList[S]): RList[S] = {
     if (remaining.isEmpty) acc
     else concatTailRec(remaining.tail, remaining.head :: acc)
   }

    concatTailRec(anotherList, this.reverse).reverse
  }

  // remove an element at a given index, return a NEW list
  override def removeAt(index: Int): RList[T] = {
    @tailrec
    def removeAtTailRec(remaining: RList[T], currentIndex: Int, acc: RList[T]): RList[T] = {
      if (currentIndex == index) acc.reverse ++ remaining.tail
      else if (remaining.isEmpty) acc.reverse
      else removeAtTailRec(remaining.tail, currentIndex + 1, remaining.head :: acc)
    }

    if (index < 0) this
    else removeAtTailRec(this, 0, RNil)
  }
}

object RList {
  def from[T](iterable: Iterable[T]): RList[T] = {
    @tailrec
    def convertToRListTailRec(remaining: Iterable[T], acc: RList[T]): RList[T] = {
      if (remaining.isEmpty) acc
      else convertToRListTailRec(remaining.tail, remaining.head :: acc)
    }

    convertToRListTailRec(iterable, RNil).reverse
  }
}

object ListProblems extends App {
  val aSmallList = 1 :: 2 :: 3 :: RNil
  val anotherSmallList = 4 :: 5 :: 6 :: RNil
  val aLargeList = RList.from(1 to 10000)

  // test get k-th
  println(aSmallList.apply(0))
  println(aSmallList.apply(1))
  println(aLargeList.apply(8735))

  // test length
  println(aSmallList.length)
  println(aLargeList.length)

  // test reverse
  println(aSmallList.reverse)
  println(aLargeList.reverse)

  // test concat
  println(aSmallList ++ anotherSmallList)

  // test removeAt
  println(aSmallList.removeAt(2))
  println(aLargeList.removeAt(13))
}
