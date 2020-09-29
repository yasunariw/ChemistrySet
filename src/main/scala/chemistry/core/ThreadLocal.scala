package chemistry.core

class ThreadLocal[T](init: => T)
extends java.lang.ThreadLocal[T] with (() => T) {
  override def initialValue: T = init
  def apply(): T = get
}
