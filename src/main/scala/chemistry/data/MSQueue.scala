// An implementation of the classic Michael-Scott queue via reagents

package chemistry.data

import chemistry.core.{Reagent, Ref, computed}

import scala.annotation.tailrec

final class MSQueue[A >: Null] {
  private abstract class Q
  private case class Node(data: A, next: Ref[Q] = Ref(Emp)) extends Q
  private final case object Emp extends Q
  private val head = Ref[Node](Node(null))
//  private val tail = Ref(head.read!())
  private var tail = head.read!()

  val enq: Reagent[A, Unit] = computed { x: A =>
    val newNode = Node(x)
    @tailrec def search: Reagent[Unit,Unit] = {
      val nextRef = tail.next
      val next = nextRef.data.get
      if (next eq null) {
        search
      } else if (next eq Emp) {
        // if tail ref is pointing to Emp, execute CAS
        nextRef.cas(Emp, newNode)
      } else {
        // otherwise, someone enqueued before me; help clean up after it and try again
        tail = next.asInstanceOf[Node]
        search
      }
    }
    search
  }

  val tryDeq: Reagent[Unit, Option[A]] = head.upd[Option[A]] {
    case Node(_, Ref(n@Node(x, _))) => (n, Some(x))
    case emp => (emp, None)
  }
  val deq: Reagent[Unit, A] = head.upd[A] {
    case Node(_, Ref(n@Node(x, _))) => (n, x)
  }
}


