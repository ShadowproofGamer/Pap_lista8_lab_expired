sealed trait LT[+A]
case class EmptyL[A](value: A) extends LT[A]
case class NodeL[A](value: A, list: List[LT[A]]) extends LT[A]

def uniqueValues[A](lt: LT[A]): Boolean = {
  def subValuesToList(lt: LT[A]): List[A] = {
    lt match {
      case EmptyL(v) => List(v)
      case NodeL(v, list: List[LT[A]]) => v :: list.flatMap( el => subValuesToList(el) )
    }
  }
  val valueList = subValuesToList(lt)
  valueList.size == valueList.toSet.size
}