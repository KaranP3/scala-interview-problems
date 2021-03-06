package com.github.karanp3.lists

import scala.annotation.tailrec
import scala.util.Random

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

  // the big 3
  def map[S](f: T => S): RList[S]
  def flatMap[S](f: T => RList[S]): RList[S]
  def filter(f: T => Boolean): RList[T]

  /**
  Medium problems
   */
  // run-length encoding
  def rle: RList[(T, Int)]

  // duplicate each element a number of times in a row
  def duplicateEach(k: Int): RList[T]

  // rotate by a number of positions to the left
  def rotate(k: Int): RList[T]

  // random sample
  def sample(k: Int): RList[T]

  /**
   * Hard problems
   */
  // sorting the list in the order defined by the Ordering object
  def insertionSort[S >: T](ordering: Ordering[S]): RList[S]
  def mergeSort[S >: T](ordering: Ordering[S]): RList[S]
  def quickSort[S >: T](ordering: Ordering[S]): RList[S]
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

  // the big 3
  override def map[S](f: Nothing => S): RList[S] = RNil

  override def flatMap[S](f: Nothing => RList[S]): RList[S] = RNil

  override def filter(f: Nothing => Boolean): RList[Nothing] = RNil

  /**
   * Medium problems
   */
  // run-length encoding
  override def rle: RList[(Nothing, Int)] = RNil

  // duplicate each element
  override def duplicateEach(k: Int): RList[Nothing] = RNil

  // rotate by a number of positions to the left
  override def rotate(k: Int): RList[Nothing] = RNil

  // random sample
  override def sample(k: Int): RList[Nothing] = RNil

  /**
   * Hard problems
   */

  // sorting
  override def insertionSort[S >: Nothing](ordering: Ordering[S]): RList[S] = RNil
  override def mergeSort[S >: Nothing](ordering: Ordering[S]): RList[S] = RNil
  override def quickSort[S >: Nothing](ordering: Ordering[S]): RList[S] = RNil
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

  // the big 3
  override def map[S](f: T => S): RList[S] = {
    @tailrec
    def mapTailRec(remaining: RList[T], acc: RList[S]): RList[S] = {
      if (remaining.isEmpty) acc.reverse
      else mapTailRec(remaining.tail, f(remaining.head) :: acc)
    }

    mapTailRec(this, RNil)
  }

  override def flatMap[S](f: T => RList[S]): RList[S] = {
    /*
      Complexity: O(Z ^ 2)
     */
    @tailrec
    def flatMapTailRec(remaining: RList[T], acc: RList[S]): RList[S] = {
      if (remaining.isEmpty) acc.reverse
      else flatMapTailRec(remaining.tail, f(remaining.head) ++ acc)
    }

    /*
      Complexity: O(N + Z)
     */
    @tailrec
    def betterFlatMap(remaining: RList[T], acc: RList[RList[S]]): RList[S] = {
      if (remaining.isEmpty) concatenateAll(acc, RNil, RNil)
      else betterFlatMap(remaining.tail, f(remaining.head).reverse :: acc)
    }

    /*
      Complexity: O(Z)
     */
    @tailrec
    def concatenateAll(elements: RList[RList[S]], currentList: RList[S], acc: RList[S]): RList[S] = {
      if (currentList.isEmpty && elements.isEmpty) acc
      else if (currentList.isEmpty) concatenateAll(elements.tail, elements.head, acc)
      else concatenateAll(elements, currentList.tail, currentList.head :: acc)
    }

    betterFlatMap(this, RNil)
  }

  override def filter(f: T => Boolean): RList[T] = {
    @tailrec
    def filterTailRec(remaining: RList[T], acc: RList[T]): RList[T] = {
      if(remaining.isEmpty) acc.reverse
      else filterTailRec(remaining.tail, if (f(remaining.head)) remaining.head :: acc else acc)
    }

    filterTailRec(this, RNil)
  }

  /**
   * Medium problems
   */
  // run-length encoding
  override def rle: RList[(T, Int)] = {
    @tailrec
    def rleTailRec(remaining: RList[T], acc: RList[(T, Int)], curr: (T, Int)): RList[(T, Int)] = {
      if (remaining.isEmpty) curr :: acc
      else if (remaining.head != curr._1) rleTailRec(remaining.tail, curr :: acc, (remaining.head, 1))
      else rleTailRec(remaining.tail, acc, (curr._1, curr._2 + 1))
    }

    rleTailRec(this.tail, RNil, (this.head, 1)).reverse
  }

  // duplicate each
  override def duplicateEach(k: Int): RList[T] =  {
    @tailrec
    def duplicateEachTailRec(remaining: RList[T], acc: RList[T], count: Int): RList[T] = {
      if (remaining.isEmpty) acc
      else if (count < k) duplicateEachTailRec(remaining, remaining.head :: acc, count + 1)
      else duplicateEachTailRec(remaining.tail, acc, 0)
    }

    duplicateEachTailRec(this, RNil, 0).reverse
  }

  // rotate list
  override def rotate(k: Int): RList[T] = {
    /*
    Complexity: O(max(N, K))
     */
    @tailrec
    def rotateTailRec(remaining: RList[T], buffer: RList[T], nRemaining: Int): RList[T] = {
      if (nRemaining == 0) remaining ++ buffer.reverse
      else if (remaining.isEmpty) rotateTailRec(this, RNil, nRemaining)
      else rotateTailRec(remaining.tail, remaining.head :: buffer, nRemaining - 1)
    }

    rotateTailRec(this, RNil, k)
  }

  // random sample
  override def sample(k: Int): RList[T] = {
    /*
    Complexity: O(N * K)
     */
    @tailrec
    def sampleTailRec(acc: RList[T], nRemaining: Int): RList[T] = {
      if (nRemaining == 0) acc
      else sampleTailRec(this(Random.nextInt(this.length)) :: acc, nRemaining - 1)
    }

    sampleTailRec(RNil, k)
  }

  /**
   * Hard problems
   */
  override def insertionSort[S >: T](ordering: Ordering[S]): RList[S] =  {
    /*
      insertSorted(4, [], [1, 2, 3, 5]) =
      insertSorted(4, [1], [2, 3, 5]) =
      insertSorted(4, [2, 1], [3, 5]) =
      insertSorted(4, [3, 2, 1], [5]) =
      [3, 2, 1].reverse ++ (4 :: [5]) =
      [1, 2, 3, 4, 5]

      Complexity: O(N)
     */
    @tailrec
    def insertSorted(element: T, before: RList[S], after: RList[S]): RList[S] = {
      if (after.isEmpty || ordering.lteq(element, after.head)) before.reverse ++ (element :: after)
      else insertSorted(element, after.head :: before, after.tail)
    }

    /*
      [3, 1, 4, 2, 5].sorted = insertSortTailRec([3, 1, 4, 2, 5], [])
      = insertSortTailRec([1, 4, 2, 5], [3])
      = insertSortTailRec([4, 2, 5], [1, 3])
      = insertSortTailRec([2, 5], [1, 3, 4])
      = insertSortTailRec([5], [1, 2, 3, 4])
      = insertSortTailRec([], [1, 2, 3, 4, 5])
      = [1, 2, 3, 4, 5]

      Complexity: O(N^2)
     */
    @tailrec
    def insertSortTailRec(remaining: RList[T], acc: RList[S]): RList[S] = {
      if (remaining.isEmpty) acc
      else insertSortTailRec(remaining.tail, insertSorted(remaining.head, RNil, acc))
    }

    insertSortTailRec(this, RNil)
  }

  // first, we want to basically split it in half
  override def mergeSort[S >: T](ordering: Ordering[S]): RList[S] = {
    @tailrec
    def merge(listA: RList[S], listB: RList[S], acc: RList[S]): RList[S] =  {
      if (listA.isEmpty) acc.reverse ++ listB
      else if (listB.isEmpty) acc.reverse ++ listA
      else if (ordering.lteq(listA.head, listB.head)) merge(listA.tail, listB, listA.head :: acc)
      else merge(listA, listB.tail, listB.head :: acc)
    }

    @tailrec
    def mergeSortTailRec(smallLists: RList[RList[S]], bigLists: RList[RList[S]]): RList[S] = {
      if (smallLists.isEmpty) {
        if (bigLists.isEmpty) RNil
        else if (bigLists.tail.isEmpty) bigLists.head
        else mergeSortTailRec(bigLists, RNil)
      } else if (smallLists.tail.isEmpty) {
        if (bigLists.isEmpty) smallLists.head
        else mergeSortTailRec(smallLists.head :: bigLists, RNil)
      }
      else {
        val first = smallLists.head
        val second = smallLists.tail.head
        val merged = merge(first, second, RNil)
        mergeSortTailRec(smallLists.tail.tail, merged :: bigLists)
      }
    }

    mergeSortTailRec(this.map((x) => x :: RNil), RNil)
  }

  override def quickSort[S >: T](ordering: Ordering[S]): RList[S] = {
    @tailrec
    def partition(list: RList[T], pivot: T, smaller: RList[T], larger: RList[T]): (RList[T], RList[T]) = {
      if (list.isEmpty) (smaller, larger)
      else if (ordering.lteq(list.head, pivot)) partition(list.tail, pivot, list.head :: smaller, larger)
      else partition(list.tail, pivot, smaller, list.head :: larger)
    }

    @tailrec
    def quickSortTailRec(remainingLists: RList[RList[T]], acc: RList[RList[T]]): RList[T] = {
      if (remainingLists.isEmpty) acc.flatMap(smallList => smallList).reverse
      else if (remainingLists.head.isEmpty) quickSortTailRec(remainingLists.tail, acc)
      else if (remainingLists.head.tail.isEmpty) quickSortTailRec(remainingLists.tail, remainingLists.head :: acc)
      else {
        val list = remainingLists.head
        val pivot = list.head
        val listToSplit = list.tail
        val (smaller, larger) = partition(listToSplit, pivot, RNil, RNil)
        quickSortTailRec(smaller :: (pivot :: RNil) :: larger :: remainingLists.tail, acc)
      }
    }

    quickSortTailRec(this :: RNil, RNil)
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
  val oneToTen = RList.from(1 to 10)

  def testEasyFunctions(): Unit = {
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

    // test the big 3
    println(aSmallList.map(x => x * 10))
    println(aSmallList.filter(x => x % 2 != 0))
  }

  def testMediumFunctions(): Unit = {
    // test run-length encoding
    println((1 :: 1 :: 1 :: 2 :: 3 :: 3 :: 4 :: 5 :: 5 :: 5 :: RNil).rle)
    println((1 :: 1 :: 2 :: 3 :: 3 :: 3 :: 3 :: 4 :: 4 :: 4 :: 5 :: 6 :: RNil).rle)

    // test duplicate each
    println(aSmallList.duplicateEach(4))

    // test rotate
    for {
      i <- 1 to 20
    } println(oneToTen.rotate(i))

    // test random sample
    println(oneToTen.sample(50))

    // test better flatMap
    println(aSmallList.flatMap(x => x :: 2 * x :: RNil))
    val time = System.currentTimeMillis()
    aLargeList.flatMap(x => x :: 2 * x :: RNil)
    println(System.currentTimeMillis() - time)
  }

  def testHardFunctions(): Unit = {
    val anUnorderedList = 3 :: 1 :: 2 :: 4 :: 5 :: RNil
    val ordering = Ordering.fromLessThan[Int](_ < _)
    val listToSort = aLargeList.sample(10)

    // insertion sort
    println(anUnorderedList.insertionSort(ordering))
    println(listToSort.insertionSort(ordering))

    // merge sort
    println(listToSort.mergeSort(ordering))

    // quick sort
    println(listToSort.quickSort(ordering))
  }

  testHardFunctions()
}
