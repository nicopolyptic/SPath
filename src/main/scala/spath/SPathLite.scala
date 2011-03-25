package spath

import collection.mutable.HashMap

trait SPathLite[T <: AnyRef] extends SPath[T] {

  def childAxis : T => IndexedSeq[T]

  override final def children  = (n : T) => {
    val result = childAxis(n)
    for (c <- result)
      parentRelation.put(IdentityWrapper(c), n)
    result
  }

  override def parent = (n : T) => {
    val p = parentRelation.get(IdentityWrapper(n))
    p match {case Some(m) => List(m) case None => empty}
  }

  private val parentRelation = new HashMap[IdentityWrapper[T],T]

  private var depth = 0

  override def startEvaluation = depth += 1

  override def endEvaluation = {
    depth -=  1
    if (depth == 0)
       parentRelation.clear
  }
}
