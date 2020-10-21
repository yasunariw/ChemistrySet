package chemistry.examples

import chemistry.core.Reagent
import org.scalatest.{FunSpec, Matchers}

trait ConcurrentSetSpec { this: FunSpec with Matchers =>
  val NUM_THREADS = 4
  val INPUT_SIZE = 6000

  type set[A >: Null] <: {
    val add: Reagent[A,Boolean]
    val remove: Reagent[A,Boolean]
    val contains: Reagent[A,Boolean]
  }
  protected def newSet[A >: Null](): set[A]

  describe("Concurrent Set") {
    it("should behave correctly") {
      val set = newSet[Integer]()
      val (input, adders, checkers, remover) = mkThreads(set)
      val threads = adders ++ checkers ++ List(remover)

      for (t <- adders) t.start()
      for (t <- adders) t.join()

      remover.start()
      for (t <- checkers) t.start()
      for (t <- checkers) t.join()
      remover.join()

      val allWitnessed = checkers.map(_.witnessed).toSet.flatten
      val allRemoved = remover.removed

      for (r <- allRemoved) {
        assert(!(set.contains ! r))
      }

      for (r <- allWitnessed if !allRemoved.contains(r)) {
        assert(!(set.contains ! r))
      }

      for (e <- input) {
        assert(!(set.contains ! e) && allRemoved.contains(e) ||
          set.contains ! e && !allRemoved.contains(e)
        )
      }
    }
  }

  def mkThreads(s: set[Integer]): (List[Integer], List[Adder], List[Checker], Remover) = {
    val inputs = (for (i <- 0 until NUM_THREADS) yield {
      val start = i * INPUT_SIZE
      val end = (i + 1) * INPUT_SIZE - 1
      (start to end).map(i => Integer.valueOf(i)).toList
    }).toList

    val adders = for (in <- inputs) yield new Adder(s, in)
    val checkers = for (in <- inputs) yield new Checker(s, in)
    val remover = new Remover(s, inputs.flatten)
    (inputs.flatten, adders, checkers, remover)
  }


  class Adder(s: set[Integer], input: List[Integer]) extends Thread {
    override def run(): Unit = {
      for (i <- input) s.add ! i
    }
  }

  class Remover(s: set[Integer], input: List[Integer]) extends Thread {
    var removed: Set[Integer] = Set.empty
    override def run(): Unit = {
      for (i <- input) {
        if (s.remove ! i) removed = removed + i
      }
    }
  }

  class Checker(s: set[Integer], elems: List[Integer]) extends Thread {
    var witnessed: Set[Integer] = Set.empty
    override def run(): Unit = {
      for (i <- elems) {
        if (s.contains ! i) {
          witnessed = witnessed + i
        }
      }
    }
  }

  describe("Concurrent Set") {
    it("should remove as false when empty") {
      val s = newSet[Integer]()
      s.remove ! 5 shouldBe false
    }
    it("should remove as true when has value") {
      val s = newSet[Integer]()
      s.add ! 5 shouldBe true
      s.remove ! 5 shouldBe true
    }
  }

}

class LockFreeListSpec extends FunSpec with Matchers with ConcurrentSetSpec {
  type set[A >: Null] = LockFreeList[A]
  protected def newSet[A >: Null]() = new LockFreeList[A]()
}