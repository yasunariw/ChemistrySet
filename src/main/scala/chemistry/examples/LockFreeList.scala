package chemistry.examples

import chemistry.core.{Reagent, Ref, computed, lift}

import scala.annotation.tailrec

final class LockFreeList[T >: Null] {
  case class MarkableRef[A <: AnyRef](initialRef: A, initialMarked: Boolean) {
    private val pair = Ref((initialRef, initialMarked))

    def getReference: Reagent[Unit,A] = pair.read.map(_._1)

    def isMarked: Reagent[Unit,Boolean] = pair.read.map(_._2)

    def get: Reagent[Unit,(A,Boolean)] = pair.read

    def cas(expectedRef: A, newRef: A, expectedMark: Boolean, newMark: Boolean): Reagent[Unit, Unit] = pair.upd[Unit] {
      case actual if actual._1 == expectedRef && actual._2 == expectedMark => ((newRef, newMark), ())
    }

    def tryCas(expectedRef: A, newRef: A, expectedMark: Boolean, newMark: Boolean): Reagent[Unit,Boolean] = pair.upd[Boolean] {
      case actual if actual._1 == expectedRef && actual._2 == expectedMark => ((newRef, newMark), true)
      case actual => (actual, false)
    }

    def attemptMark(expectedRef: A, newMark: Boolean): Reagent[Unit, Boolean] = pair.upd {
      case actual if actual._1 == expectedRef =>((actual._1, newMark), true)
      case actual => (actual, false)
    }

    @inline def upd[B,C](f: ((A, Boolean),B) => ((A, Boolean),C)): Reagent[B,C] = pair.upd(f)
  }


  /**
   * list node
   */
  case class Node(item: T, next: MarkableRef[Node]) {
    private var _key: Option[Int] = None
    def key: Int = if (_key.isEmpty) item.hashCode() else _key.get
  }
  object Node {
    def apply(key: Int): Node = {
      val n = Node(null, MarkableRef(null, initialMarked = false))
      n._key = Some(key)
      n
    }
  }


  class Window(var pred: Node, var curr: Node)

  private val head = Ref(Node(Integer.MIN_VALUE))
  private val tail = Ref(Node(Integer.MAX_VALUE))
  head.data.get.next.tryCas(null, tail.data.get, false, false) ! ()

  val add: Reagent[T, Boolean] = computed { item: T =>
    @tailrec
    def visit: Reagent[Unit, Boolean] = {
      val key = item.hashCode()
      val window = find(key) ! ()
      val pred = window.pred
      val curr = window.curr
      if (curr.key == key) {
        lift(_ => false)
      } else {
        val node = new Node(item, MarkableRef(curr, false))
        if (pred.next.tryCas(curr, node, false, false) ! ()) {
          lift(_ => true)
        } else {
          visit
        }
      }
    }
    visit
  }

  val remove: Reagent[T, Boolean] = computed { item: T =>
    val key = item.hashCode

    @tailrec
    def visit: Reagent[Unit, Boolean] = {
      val window = find(key) ! ()
      val pred = window.pred
      val curr = window.curr
      if (curr.key != key) {
        lift(_ => false)
      } else {
        val (succ, _) = curr.next.get ! ()
        val snip = curr.next.attemptMark(succ, true) ! ()
        if (snip) {
          pred.next.tryCas(curr, succ, false, false)
          lift(_ => true)
        } else {
          visit
        }
      }
    }
    visit
  }

  val contains: Reagent[T, Boolean] = computed { item: T =>
    val key = item.hashCode
    find(key).map(w => w.curr.key == key)
  }

  object Impossible extends Exception

  def find(key: Int): Reagent[Unit, Window] = {
    var pred, curr, succ: Node = null
    var marked = false
    var snip = false
    while (true) {
      var retry = false
      pred = head.read ! ()
      curr = (pred.next.get ! ())._1
      while (!retry) {
        val res = curr.next.get ! ()
        succ = res._1
        marked = res._2
        while (marked && !retry) {
          snip = pred.next.tryCas(curr, succ, false, false) ! ()
          if (!snip) {
            retry = true
          } else {
            curr = (pred.next.get ! ())._1
            val res = curr.next.get ! ()
            succ = res._1
            marked = res._2
          }
        }
        if (!retry) {
          if (curr.key >= key) {
            return lift(_ => new Window(pred, curr))
          }
          pred = curr
          curr = succ
        }
      }
    }
    throw Impossible
  }
}
