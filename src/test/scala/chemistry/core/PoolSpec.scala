package chemistry.core

import java.util.concurrent.atomic.AtomicReference

import org.scalatest.{FunSpec, Matchers}

import scala.language.reflectiveCalls

trait PoolSpec { this: FunSpec with Matchers =>
  private abstract class DelStatus
  private final case object Deleted extends DelStatus
  private final case object Active extends DelStatus
  private case class TestItem(i: Int) extends DeletionFlag {
    val deletedFlag = new AtomicReference[DelStatus](Active)
    def isDeleted: Boolean = deletedFlag.get == Deleted
    def delete: Boolean = deletedFlag.compareAndSet(Active, Deleted)
  }

  protected def newPool[A <: DeletionFlag](): Pool[A]
  protected def title: String

  private def np: Pool[TestItem] = newPool[TestItem]()

  describe("Pool") {
    it("should return a null cursor when empty") {
      assert(np.cursor === null)
    }

    it("should return a nonnull cursor when nonempty") {
      val p = np
      p.put(TestItem(2))
      assert(p.cursor !== null)
    }

    it("should contain the single item when a singleton") {
      val p = np
      p.put(TestItem(2))
      assert(p.cursor.data.i === 2)
    }

    it("should return a null cursor after removing all items") {
      val p = np
      val ti = TestItem(2)
      p.put(ti)
      ti.delete
      assert(p.cursor === null)
    }

    it("should iterate through all inserted items (in any order)") {
      val p = np
      p.put(TestItem(1))
      p.put(TestItem(2))
      val t1 = p.cursor
      val t2 = t1.next
      List(t1.data.i, t2.data.i) should contain only (1, 2)
    }
  }

}


class CircularPoolSpec extends FunSpec with Matchers with PoolSpec {
  def title = "a CircularPool"
  def newPool[A <: DeletionFlag]() = new CircularPool[A]
}
