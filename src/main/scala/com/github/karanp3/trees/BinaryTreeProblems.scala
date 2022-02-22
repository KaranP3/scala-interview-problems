package com.github.karanp3.trees

import scala.annotation.tailrec

sealed abstract class BTree[+T] {
  def value: T
  def left: BTree[T]
  def right: BTree[T]
  def isEmpty: Boolean

  /**
    * Easy problems
    */
  def isLeaf: Boolean
  def collectLeaves: List[BTree[T]]
  def leafCount: Int
}

case object BEnd extends BTree[Nothing] {
  override def value = throw new NoSuchElementException
  override def left = throw new NoSuchElementException
  override def right = throw new NoSuchElementException
  override def isEmpty: Boolean = true

  /**
    * Easy problems
    */
  override def isLeaf: Boolean = false
  override def collectLeaves: List[BTree[Nothing]] = List()
  override def leafCount: Int = 0
}

case class BNode[+T](override val value: T,
                     override val left: BTree[T],
                     override val right: BTree[T])
    extends BTree[T] {
  override def isEmpty: Boolean = false

  /**
    * Easy problems
    */
  override def isLeaf: Boolean = left.isEmpty && right.isEmpty

  override def collectLeaves: List[BTree[T]] = {
    @tailrec
    def collectLeavesTailRec(todo: List[BTree[T]],
                             leaves: List[BTree[T]]): List[BTree[T]] = {
      if (todo.isEmpty) leaves
      else if (todo.head.isEmpty) collectLeavesTailRec(todo.tail, leaves)
      else if (todo.head.isLeaf)
        collectLeavesTailRec(todo.tail, todo.head :: leaves)
      else {
        val node = todo.head
        collectLeavesTailRec(node.left :: node.right :: todo.tail, leaves)
      }
    }

    collectLeavesTailRec(List(this), List())
  }

  override def leafCount: Int = collectLeaves.length
}

object BinaryTreeProblems extends App {
  val tree = BNode(
    1,
    BNode(2, BNode(3, BEnd, BEnd), BNode(4, BEnd, BNode(5, BEnd, BEnd))),
    BNode(6, BNode(7, BEnd, BEnd), BNode(8, BEnd, BEnd))
  )

  /**
    * Easy problems
    */
  println(tree.collectLeaves.map(_.value))
  println(tree.leafCount)
}
