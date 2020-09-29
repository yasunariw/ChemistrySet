// One-time use count-down latch

package chemistry.sync

import chemistry.core.Reagent
import chemistry.data.Counter

final class CountDownLatch(count: Int) {
  private val state = new Counter(count)
  val getCount: Reagent[Unit, Int] = state.get
  val countDown: Reagent[Unit, Unit] = state.tryDec.map(_ => ())
  val await: Reagent[Unit, Int] = state.get.withFilter(_ == 0)
}
