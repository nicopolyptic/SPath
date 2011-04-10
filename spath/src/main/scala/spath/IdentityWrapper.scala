package spath


class IdentityWrapper[T <: AnyRef](val o: T) {
  override def hashCode = java.lang.System.identityHashCode(o)

  override def equals(that: Any) = that match {
    case so: IdentityWrapper[T] => so.o eq this.o case _ => false
  }
}

object IdentityWrapper {
  def apply[T <: AnyRef](o: T) = new IdentityWrapper(o)
}