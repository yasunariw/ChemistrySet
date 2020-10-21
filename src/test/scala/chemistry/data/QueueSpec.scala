package chemistry.data

import chemistry.TestUtil
import chemistry.core.Reagent
import org.scalatest.{FunSpec, Matchers}

trait QueueSpec { this: FunSpec with Matchers =>
  type queue[A >: Null] <: {
    val tryDeq: Reagent[Unit,Option[A]]
    val enq: Reagent[A,Unit]
    val deq: Reagent[Unit,A]
  }

  protected def newQueue[A >: Null](): queue[A]

  describe("Queue") {
    it("should tryDeq as None when empty") {
      val q = newQueue[java.lang.Integer]()
      q.tryDeq ! () shouldBe empty
    }

    it("should tryDeq as Some _ when full") {
      val q = newQueue[java.lang.Integer]()
      q.enq ! 1
      q.tryDeq ! () should not be empty
    }

    it("should tryDeq as None after tryDequeueing") {
      val q = newQueue[java.lang.Integer]()
      q.enq ! 1
      q.tryDeq ! ()
      q.tryDeq ! () shouldBe empty
    }

    it("should tryDeq in order") {
      val q = newQueue[java.lang.Integer]()
      q.enq ! 1
      q.enq ! 2
      q.tryDeq ! () should contain (1)
      q.tryDeq ! () should contain (2)
    }

    it("should enqueue from multiple threads in locally-ordered way") {
      val testResults = for (_ <- 1 to 10) yield concTest
      all (testResults) shouldBe (true)
    }

    def concTest: Boolean = {
      val max = 100000

      val q = newQueue[java.lang.Integer]()
      TestUtil.spawnAndJoin (List(
        () => for (i <- 1 to max) q.enq ! i,
        () => for (i <- max+1 to 2*max) q.enq ! i
      ))

      val outcome = (1 to 2*max).flatMap(_ => q.tryDeq ! ())

      outcome.size shouldBe 2*max

      val left  = for (i <- outcome if i <= max) yield i
      val right = for (i <- outcome if i >  max) yield i
      val comp  = left ++ right
      val eqs   = for ((i,j) <- comp zip (1 to 2*max)) yield i == j

      eqs.forall(_ == true)
    }
  }
}

class MSQueueSpec extends FunSpec with Matchers with QueueSpec {
  type queue[A >: Null] = MSQueue[A]
  protected def newQueue[A >: Null]() = new MSQueue[A]()
}

class MSQueueAltSpec extends FunSpec with Matchers with QueueSpec {
  type queue[A >: Null] = MSQueueAlt[A]
  protected def newQueue[A >: Null]() = new MSQueueAlt[A]()
}
