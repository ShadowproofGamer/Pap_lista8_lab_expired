sealed trait BT[+A]
case class Element(value: Int) extends BT[Int]
case class Node[+A](elem: Element, left: BT[A], right: BT[A]) extends BT[A]