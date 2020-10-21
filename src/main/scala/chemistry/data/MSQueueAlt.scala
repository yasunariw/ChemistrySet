package chemistry.data

import chemistry.core.{Reagent, Ref, computed, postCommit}

import scala.annotation.tailrec

class MSQueueAlt[A >: Null] {
  object Impossible extends Exception

  trait Node
  case object Nil extends Node
  case class Next(data: A, next: Ref[Node]) extends Node

  val head: Ref[Node] = Ref(Next(null, Ref(Nil)))
  val tail: Ref[Node] = Ref(head.read ! ())

  val deq: Reagent[Unit, A] = head.upd[A] {
    case Nil => throw Impossible
    case Next(_, Ref(n@Next(x, _))) => (n, x)
  }

  val tryDeq: Reagent[Unit, Option[A]] = head.upd[Option[A]] {
    case Nil => throw Impossible
    case n@Next(_, Ref(Nil)) => (n, None)
    case Next(_, Ref(n@Next(x, _))) => (n, Some(x))
  }

  val enq: Reagent[A, Unit] = computed { x: A =>
    @tailrec
    def search(n: Node, tail: Ref[Node]): Reagent[Unit, Unit] = tail.read ! () match {
      case Nil => throw Impossible
      case ov@Next(_, r) =>
        r.read ! () match {
          case Nil =>
            r.cas(Nil, n) >> postCommit(_ => tail.data.compareAndSet(ov, n))
          case nv@Next(_, _) =>
            tail.data.compareAndSet(ov, nv)
            search(n, tail)
        }
    }
    search(Next(x, Ref(Nil)), tail)
  }
}
