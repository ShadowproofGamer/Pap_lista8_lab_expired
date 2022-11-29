sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem: List[Double], left: BT[A], right: BT[A]) extends BT[A]
case class SumNode[+A](elem: Double, left: BT[A], right: BT[A]) extends BT[A]




def listsIntoSum[A](tree:BT[A]):BT[A] = {
  def listsIntoSum_chaos[A](remaining:List[BT[A]], result:BT[A]):BT[A] = {
    remaining match
      case List() => result
      case List(Empty) => result
      case Empty::t => listsIntoSum_chaos(t, result)
      case Node(x, y, z)::t if result==Empty => SumNode(x.sum, listsIntoSum_chaos(List(y), Empty), listsIntoSum_chaos(List(z), Empty))
  }

  def listsIntoSum_rec[A](remaining: List[BT[A]], resultQ: List[Double]): List[Double] = {
    remaining match
      case List() => resultQ
      case List(Empty) => resultQ
      case Empty :: t => listsIntoSum_rec(t, resultQ)
      case Node(x, y, z) :: t if resultQ.isEmpty => listsIntoSum_rec(List(y, z), (x.sum)::resultQ)
  }
  listsIntoSum_chaos(List(tree), Empty)
}

val tt = Node(List(1,3,5,4), Node(List(1,3,2,4), Empty, Empty), Node(List(1), Empty, Node(List(1,3,2,4), Empty, Empty)))
listsIntoSum(tt)