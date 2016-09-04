package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[Int]
    h <- frequency((1, Gen.const(empty)), (9, genHeap))
  } yield insert(n, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

    property("gen1") = forAll { (h: H) =>
      val m = if (isEmpty(h)) 0 else findMin(h)
      findMin(insert(m, h)) == m
    }

    property("min1") = forAll { a: Int =>
      val h = insert(a, empty)
      findMin(h) == a
    }

    property("findMin1") = forAll { (b: Int, a: Int) =>
      val min = math.min(a, b)
      val max = math.max(a, b)
      val h1 = insert(b, empty)
      val h2 = insert(a, h1)
      findMin(h2) == min
    }

     property("deleteMin1") = forAll { (a: Int) =>
      val h1 = insert(a, empty)
      deleteMin(h1) == empty
    }

    property("meld12") = forAll { (aList: List[Int], bList: List[Int]) =>
      if (aList.isEmpty && bList.isEmpty) {
        true
      } else {
        val min = (aList ::: bList) .reduce(math.min(_, _))
        val hA = aList.foldRight(empty)((o1, o2) => insert(o1, o2))
        val hB = bList.foldRight(empty)(insert(_, _))
        val h5 = meld(hA, hB)
        findMin(h5) == min
      }
    }

    property("sorted") = forAll { (aList: List[Int]) =>
      def iter(curList: List[Int], curH: H): Boolean = {
        (curList, curH) match {
          case (Nil, empty) => true
          case (lx :: lxx, curHx) => {
            if (curHx == empty) {
              false
            } else {
              val hMin = findMin(curHx)
              if (hMin == lx) iter(lxx, deleteMin(curHx))
              else false
            }
          }
        }
      }
      
      if (aList.isEmpty) {
        true
      } else {
        val sortedList = aList.sorted
        val hA = aList.foldRight(empty)((o1, o2) => insert(o1, o2))
        iter(sortedList, hA)
      }
    }

}
