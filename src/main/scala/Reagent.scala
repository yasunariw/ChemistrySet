// The core reagent implementation and accompanying combinators

package chemistry

import scala.annotation.tailrec
import java.util.concurrent.locks._
import chemistry.Util.Implicits._

private sealed abstract class BacktrackCommand
private case object ShouldBlock extends BacktrackCommand 
private case object ShouldRetry extends BacktrackCommand 

private object OfferFail extends Exception

abstract class Reagent[-A, +B] {
  // returns either a BacktrackCommand or a B
  private[chemistry] def tryReact(a: A, rx: Reaction): Any
  protected def makeOfferI(a: A, offer: Offer[B]): Unit
  protected def composeI[C](next: Reagent[B,C]): Reagent[A,C]
  private[chemistry] def alwaysCommits: Boolean
  private[chemistry] def maySync: Boolean

  @inline private[chemistry] final def makeOffer(a: A, offer: Offer[B]) {
    // abort early if offer has already been consumed
    if (offer.isActive) makeOfferI(a, offer)
  }
  final def compose[C](next: Reagent[B,C]): Reagent[A,C] = next match {
    case Commit() => this.asInstanceOf[Reagent[A,C]] // B = C
    case _ => composeI(next)
  }

  final def !(a: A): B = {    
    def block: B = {
/*
      val waiter = new Waiter[B](true)
      val initRX = waiter.rxForConsume
      while (true) {
	waiter.reset
	makeOffer(a, waiter)

	tryReact(a, initRX) match {
	  case ShouldRetry => throw Util.Impossible
	  case ShouldBlock => throw Util.Impossible
	  case ans         => return ans.asInstanceOf[B]
	}
      }
*/
      throw Util.Impossible
    }

/*
	      case ShouldBlock => 
		if (blocking) 
		  LockSupport.park(waiter) 
		else waiter.consume !? () match {
		  case None    => if (waiter.isActive) backoff.once()
		  case Some(_) => return slowPath(true)
		}
*/

    def offer: B = {
      val backoff = new Backoff
      // scalac can't do @tailrec here, due to exception handling
      val waiter = new Waiter[B](false)
      while (true) {
	waiter.reset
	makeOffer(a, waiter)
	
	// var spins = (Chemistry.procs * offerSpinBase) << backoff
	// while (waiter.isActive && spins > 0) spins -= 1

	// val timeout = Chemistry.procs << (backoff + 5)
	// val t = System.nanoTime
	// while (waiter.isActive && System.nanoTime - t < timeout) {}

	backoff.once(waiter.isActive)

	waiter.abort match {
	  case Some(b) => return b.asInstanceOf[B]
	  case _ => tryReact(a, Inert) match {
	    case ShouldRetry => {}
	    case ShouldBlock => return block
	    case ans         => return ans.asInstanceOf[B]
	  }
	} 
      }
      throw Util.Impossible
    }

    def withBackoff: B = {
      var backoff = 0
//      val backoff = new Backoff
//      backoff.once
      // scalac can't do @tailrec here, due to exception handling
      while (true) {
	tryReact(a, Inert) match {
//	  case ShouldRetry if maySync && backoff.count > 2 => 
//	    return offer
	  case ShouldRetry => backoff += 1
	  case ShouldBlock => return block
	  case ans         => return ans.asInstanceOf[B]
	}
      }
      throw Util.Impossible
    }
    
    tryReact(a, Inert) match {
//      case ShouldRetry            => offer
//      case ShouldRetry if maySync => offer
      case ShouldRetry            => withBackoff
      case ShouldBlock		  => block
      case ans			  => ans.asInstanceOf[B]
    }
  }

  @inline final def !?(a:A) : Option[B] = {
    tryReact(a, Inert) match {
      case ShouldRetry => None	// should we actually retry here?  if we do,
				// more informative: a failed attempt entails
				// a linearization where no match was
				// possible.  but could diverge...
      case ShouldBlock => None
      case ans         => Some(ans.asInstanceOf[B])
    }
  }

  final def dissolve(a: A) {
    // todo
  }

  @inline final def flatMap[C](k: B => Reagent[Unit,C]): Reagent[A,C] = 
    compose(computed(k))
  @inline final def map[C](f: B => C): Reagent[A,C] = 
    compose(lift(f))
 @inline final def >>[C](next: Reagent[Unit,C]): Reagent[A,C] = 
   compose(lift((_:B) => ()).compose(next))
  @inline final def mapFilter[C](f: PartialFunction[B, C]): Reagent[A,C] =
    compose(lift(f))
  @inline final def withFilter(f: B => Boolean): Reagent[A,B] =
    compose(lift((_: B) match { case b if f(b) => b }))
  @inline final def <+>[C <: A, D >: B](that: Reagent[C,D]): Reagent[C,D] = 
    choice(this, that)
  @inline final def >=>[C](k: Reagent[B,C]): Reagent[A,C] =
    compose(k)
}

object ret { 
  private final case class Ret[A,B](pure: A, k: Reagent[A,B]) 
		     extends Reagent[Any,B] {
    def tryReact(x: Any, rx: Reaction): Any = 
      k.tryReact(pure, rx)
    def makeOfferI(a: Any, offer: Offer[B]) =
      k.makeOffer(pure, offer)
    def composeI[C](next: Reagent[B,C]) = Ret(pure, k.compose(next))
    def alwaysCommits = k.alwaysCommits
    def maySync = k.maySync
  }
  @inline final def apply[A](pure: A): Reagent[Any,A] = Ret(pure, Commit[A]())  
}

// Not sure whether this should be available as a combinaor
// object retry extends Reagent[Any,Nothing] {
//   final def tryReact[A](a: Any, rx: Reaction, k: K[Nothing,A]): A = 
//     throw ShouldRetry
// }

private final case class Commit[A]() extends Reagent[A,A] {
  @inline def tryReact(a: A, rx: Reaction): Any = 
    if (rx.tryCommit) a else ShouldRetry
  @inline def makeOfferI(a: A, offer: Offer[A]) {}
  @inline def composeI[B](next: Reagent[A,B]) = next
  @inline def alwaysCommits = true
  @inline def maySync = false
}

object never extends Reagent[Any, Nothing] {
  def tryReact(a: Any, rx: Reaction): Any = 
    ShouldBlock
  def makeOfferI(a: Any, offer: Offer[Nothing]) {}
  def composeI[A](next: Reagent[Nothing, A]) = never
  def alwaysCommits = false
  def maySync = false
}

object computed {
  private final case class Computed[A,B,C](c: A => Reagent[Unit,B], 
					   k: Reagent[B,C]) 
		     extends Reagent[A,C] {
    def tryReact(a: A, rx: Reaction): Any = 
      c(a).compose(k).tryReact((), rx)
    def makeOfferI(a: A, offer: Offer[C]) =
      c(a).compose(k).makeOffer((), offer)
    def composeI[D](next: Reagent[C,D]) = Computed(c, k.compose(next))
    def alwaysCommits = false
    def maySync = true
  }
  @inline def apply[A,B](c: A => Reagent[Unit,B]): Reagent[A,B] = 
    Computed(c, Commit[B]())
}

object lift {
  private final case class Lift[A,B,C](f: PartialFunction[A,B], 
				       k: Reagent[B,C]) 
		     extends Reagent[A,C] {
    def tryReact(a: A, rx: Reaction): Any =
      if (f.isDefinedAt(a)) 
	k.tryReact(f(a), rx) 
      else ShouldBlock
    def makeOfferI(a: A, offer: Offer[C]) = 
      if (f.isDefinedAt(a)) k.makeOffer(f(a), offer)
    def composeI[D](next: Reagent[C,D]) = Lift(f, k.compose(next))
    def alwaysCommits = k.alwaysCommits
    def maySync = k.maySync
  }
  @inline def apply[A,B](f: PartialFunction[A,B]): Reagent[A,B]  = 
    Lift(f, Commit[B]())
}

object choice {
  private final case class Choice[A,B](r1: Reagent[A,B], r2: Reagent[A,B]) 
		     extends Reagent[A,B] {
    @inline def tryReact(a: A, rx: Reaction): Any = 
      r1.tryReact(a, rx) match {
	case ShouldRetry => 
	  r2.tryReact(a, rx) match {
	    case ShouldRetry => ShouldRetry
	    case ShouldBlock => ShouldRetry // retry since r1 could
	    case ans         => ans 
	  }
	case ShouldBlock => 
	  r2.tryReact(a, rx) // all backtracking falls thru
	case ans => ans
      }
    @inline def makeOfferI(a: A, offer: Offer[B]) {
      r1.makeOffer(a, offer)
      r2.makeOffer(a, offer)
    }
    @inline def composeI[C](next: Reagent[B,C]) = 
      Choice(r1.compose(next), r2.compose(next))
    @inline def alwaysCommits = r1.alwaysCommits && r2.alwaysCommits
    @inline def maySync = r1.maySync || r2.maySync
  }
  @inline def apply[A,B](r1: Reagent[A,B], r2: Reagent[A,B]): Reagent[A,B] =
    Choice(r1, r2)
}

object postCommit {
  private final case class PostCommit[A,B](pc: A => Unit, k: Reagent[A,B])
		     extends Reagent[A,B] {
    def tryReact(a: A, rx: Reaction): Any = 
      k.tryReact(a, rx.withPostCommit((_:Unit) => pc(a)))
    def makeOfferI(a: A, offer: Offer[B]) =
      k.makeOffer(a, offer)
    def composeI[C](next: Reagent[B,C]) = PostCommit(pc, k.compose(next))
    def alwaysCommits = k.alwaysCommits
    def maySync = k.maySync
  }
  @inline def apply[A](pc: A => Unit): Reagent[A,A] = 
    PostCommit(pc, Commit[A]())
}
