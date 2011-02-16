package sq

import collection.mutable.HashMap

trait SQLite[T <: AnyRef] extends SQ[T] {

  /**
   * New abstract function to be overridden. Replaces children.
   */
  def childAxis : T => IndexedSeq[T]

  /**
   * Implement the children function to delegate to the user-defined
   * childAxis function and also store away the parent relation.
   */
  override def children  = (n : T) => {
    val result = childAxis(n)
    for (c <- result) parentRelation.put(c, n)
    result
  }

  override def parent = (n : T) => parentRelation.get(n)

  /**
   *  The parent axis relation generated at evaluation-time.
   */
  private val parentRelation = new HashMap[T,T]

  /**
   * The number of evaluations on the call stack.
   */
  private var depth : Int = 0

  override def startEvaluation = {
    depth = depth + 1
  }

  override def endEvaluation = {
    depth = depth - 1
    if (depth == 0)
       parentRelation.clear
  }
}
