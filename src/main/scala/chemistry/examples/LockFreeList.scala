package chemistry.examples

import chemistry.core.{Reagent, Ref, computed, conditional, lift, retryAlways}

import scala.annotation.tailrec

final class LockFreeList[T >: Null]() {
  /**
   * list node
   */
  case class Node(item: T, next: Ref[Node]) {
    private var _key: Option[Int] = None
    def key: Int = if (_key.isEmpty) item.hashCode() else _key.get
  }
  object Emp extends Node(null, null)
  object Node {
    def apply(key: Int): Node = {
      val n = Node(null, Ref(Emp))
      n._key = Some(key)
      n
    }
  }

  private val head = Node(Integer.MIN_VALUE)
  private val tail = Node(Integer.MAX_VALUE)
  head.next.cas(Emp, tail) ! ()

  val add: Reagent[T, Boolean] = traverse((cond, item, pred, curr) => {
    if (!cond) {
      pred.next.cas(curr, new Node(item, Ref(curr))).map(_ => true)
    } else {
      lift(_ => true)
    }
  })

  val remove: Reagent[T, Boolean] = traverse((cond, item, pred, curr) => {
    if (cond) {
      pred.next.cas(curr, curr.next.read ! ()).map(_ => true)
    } else {
      lift(_ => cond)
    }
  })
  val contains: Reagent[T, Boolean] = traverse((cond, _, _, _) => lift(_ => cond))

  private def traverse[R](action: (Boolean, T, Node, Node) => Reagent[Unit, R]): Reagent[T, R] = {
    @tailrec
    def validate(entry: Node, pred: Node, curr: Node): Boolean = {
      if (entry.key <= pred.key) {
        if (entry eq pred) {
          pred.next.read ! () eq curr
        } else {
          validate(entry.next.read ! (), pred, curr)
        }
      } else {
        false
      }
    }

    @tailrec
    def find(item: T, key: Int, pred: Node): (Node, Node) = {
      val next = pred.next.read ! ()
      if (next.key <= key && next.item != item) {
        find(item, key, next)
      } else {
        (pred, next)
      }
    }

    computed {item: T =>
      val key = item.hashCode()
      val (pred, curr) = find(item, key, head)
      if (validate(head, pred, curr)) {
        action(curr.item == item, item, pred, curr)
      } else {
        retryAlways
      }
    }
  }
}
