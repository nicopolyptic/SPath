package spath

import collection.mutable.HashMap

trait SPathLite[T <: AnyRef] extends SPath[T] {

  def childAxis : T => IndexedSeq[T]

  override final def children  = n => {
    val result = childAxis(n)
    for (c <- result)
      parentRelation.put(IdentityWrapper(c), n)
    result
  }

  override def parent = n => {
    val p = parentRelation.get(IdentityWrapper(n))

    p match {case Some(m) => List(m) case None => empty}
  }

  protected val parentRelation = new HashMap[IdentityWrapper[T],T]

  override def endEvaluation = {
    super.endEvaluation
    if (depth == 0 && false) // TODO and the document being queried is not indexed
       parentRelation.clear
  }
}
