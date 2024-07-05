import scala.annotation.tailrec

trait Comparator[T] {
  def compare(ob1: T, ob2: T): Int
}

abstract class L[+A] {
  def head: A
  def tail: L[A]
  def isEmpty: Boolean
  def add[B >: A](element: B): L[B]
  def size: Int
  def sort[B >: A](comparator: Comparator[B]): L[B]
  def size02: Int
  def selectionSort[B >: A](comparator: Comparator[B]): L[B]
  def binarySearch[B >: A](comparator: Comparator[B], element: B): Option[Int]
}

case object Nil extends L[Nothing] {
  def head: Nothing = throw new NoSuchElementException()
  def tail: L[Nothing] = Nil
  def isEmpty: Boolean = true
  def add[B >: Nothing](element: B): L[B] = new ConcreteL[B](element, this)
  def size: Int = 0
  def sort[B >: Nothing](comparator: Comparator[B]): L[B] = Nil
  def size02: Int = 0
  def selectionSort[B >: Nothing](comparator: Comparator[B]): L[B] = Nil
  def binarySearch[B >: Nothing](comparator: Comparator[B], element: B): Option[Int] = None
}

case class ConcreteL[+A](h: A, t: L[A]) extends L[A] {
  def head: A = h
  def tail: L[A] = t
  def isEmpty: Boolean = false
  def add[B >: A](element: B): L[B] = new ConcreteL[B](element, this)

  def size: Int = {
    @tailrec
    def sizeHelper(list: L[A], acc: Int = 1): Int = {
      if (list.tail.isEmpty) acc
      else sizeHelper(list.tail, acc + 1)
    }
    sizeHelper(this)
  }

  def size02: Int = 1 + tail.size02

  // insertion sort
  def sort[B >: A](comparator: Comparator[B]): L[B] = {
    def insert(x: B, sortedTail: L[B]): L[B] = {
      if(sortedTail.isEmpty) new ConcreteL(x, Nil)
      else if (comparator.compare(x, sortedTail.head) <= 0) new ConcreteL(x, sortedTail)
      else new ConcreteL(sortedTail.head, insert(x, sortedTail.tail))
    }
    val sortedTail = t.sort(comparator)
    insert(h, sortedTail)
  }

  // selection sort
  def selectionSort[B >: A](comparator: Comparator[B]): L[B] = {
    @tailrec
    def findMin(list: L[B], currentMin: B): B = {
      if (list.isEmpty) currentMin
      else {
        val newMin = if (comparator.compare(list.head, currentMin) < 0) list.head else currentMin
        findMin(list.tail, newMin)
      }
    }

    def removeElement(list: L[B], element: B): L[B] = {
      if (list.isEmpty) Nil
      else if (list.head == element) list.tail
      else new ConcreteL[B](list.head, removeElement(list.tail, element))
    }

    if (this.isEmpty) Nil
    else {
      val minElement = findMin(this, this.head)
      val remainingList = removeElement(this, minElement)
      new ConcreteL(minElement, remainingList.selectionSort(comparator))
    }
  }

  def binarySearch[B >: A](comparator: Comparator[B], element: B): Option[Int] = {
    @tailrec
    def getElementAtIndex(list: L[B], index: Int): Option[B] = {
      if (index < 0 || list.isEmpty) None
      else if (index == 0) Some(list.head)
      else getElementAtIndex(list.tail, index - 1)
    }

    @tailrec
    def binarySearchHelper(low: Int, high: Int): Option[Int] = {
      if (low > high) None
      else {
        val mid = (low + high) / 2
        getElementAtIndex(this, mid) match {
          case None => None
          case Some(midElement) =>
            val cmp = comparator.compare(element, midElement)
            if (cmp == 0) Some(mid)
            else if (cmp < 0) binarySearchHelper(low, mid - 1)
            else binarySearchHelper(mid + 1, high)
        }
      }
    }

    binarySearchHelper(0, size - 1)
  }
}

object LTest extends App {
  val list02: L[Int] = new ConcreteL(6, new ConcreteL(5, new ConcreteL(4, new ConcreteL(7, Nil))))

  println("List size: ")
  println(list02.size)

  val ascComparator = new Comparator[Int] {
    def compare(ob1: Int, ob2: Int): Int = {
      ob1.compareTo(ob2)
    }
  }

  val descComparator = new Comparator[Int] {
    def compare(ob1: Int, ob2: Int): Int = {
      ob2.compareTo(ob1)
    }
  }

  println("Insertion sort")
  println(list02.sort(ascComparator))
  println(list02.sort(descComparator))

  println("Selection sort: ")
  println(list02.selectionSort(ascComparator))
  println(list02.selectionSort(descComparator))

  val list03 = list02.selectionSort(ascComparator)

  println("Binary search: ")
  println(list03.binarySearch(ascComparator, 6).get)
}
