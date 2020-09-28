import chemistry.core.Reagent
import chemistry.data.{EliminationStack, TreiberStack}
import org.scalatest.{FunSpec, Matchers}

trait StackSpec { this: FunSpec with Matchers =>
  type stack[A >: Null] <: {
    val tryPop: Reagent[Unit,Option[A]]
    val push: Reagent[A,Unit]
    val pop: Reagent[Unit,A]
  }
  protected def newStack[A >: Null](): stack[A]

  describe("Stack") {
    it("should tryPop as None when empty") {
      val s = newStack[java.lang.Integer]()
      s.tryPop ! () shouldBe empty
    }

    it("should tryPop as Some _ when full") {
      val s = newStack[java.lang.Integer]()
      s.push ! 1;
      s.tryPop ! () should not be empty
    }

    it("should tryPop as None after emptying") {
      val s = newStack[java.lang.Integer]()
      s.push ! 1
      s.tryPop ! ()
      s.tryPop ! () shouldBe empty
    }


    it("should tryPop in reverse order") {
      val s = newStack[java.lang.Integer]()
      s.push ! 1
      s.push ! 2
      s.tryPop ! () should contain (2)
      s.tryPop ! () should contain (1)
    }

    it("should push from multiple threads in locally-ordered way") {
      val testResults = for (_ <- 1 to 10) yield concTest
      all (testResults) shouldBe (true)
    }

    def concTest: Boolean = {
      val max = 100000
      val s = newStack[java.lang.Integer]()

      TestUtil.spawnAndJoin (List(
        () => for (i <- 1 to max) s.push ! i,
        () => for (i <- max+1 to 2*max) s.push ! i
      ))

      val outcome = (1 to 2*max).flatMap(_ => s.tryPop ! ())

      outcome.size shouldBe 2*max

      val left  = for (i <- outcome if i <= max) yield i
      val right = for (i <- outcome if i >  max) yield i
      val comp  = left.reverse ++ right.reverse
      val eqs   = for ((i,j) <- comp zip (1 to 2*max)) yield i == j

      eqs.forall(_ == true)
    }
  }
}

class TreiberStackSpec extends FunSpec with Matchers with StackSpec {
  type stack[A >: Null] = TreiberStack[A]
  protected def newStack[A >: Null]() = new TreiberStack[A]()
}

class EliminationStackSpec extends FunSpec with Matchers with StackSpec {
  type stack[A >: Null] = EliminationStack[A]
  protected def newStack[A >: Null]() = new EliminationStack[A]()
}