// The Treiber stack with elimination-backoff, via reagents

package chemistry.data

import chemistry.core.{Chan, Reagent}

final class EliminationStack[A >: Null] {
  private val stack = new TreiberStack[A]
  private val (elimPop, elimPush) = Chan[A]()

  val push: Reagent[A,Unit] = stack.push + elimPush
  val tryPop: Reagent[Unit,Option[A]] = stack.tryPop + elimPop.map(Some(_))
  val pop: Reagent[Unit,A] = stack.pop + elimPop
}
