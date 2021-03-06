package scalan.primitives

import scalan.{ScalanExp, ScalanSeq, Scalan}
import scalan.common.Lazy

trait Blocks { self: Scalan =>
  def semicolon[A,B](left: Rep[A], right: Rep[B]): Rep[B]
 
  implicit class RepBlock[A](left: Rep[A]) { 
    def |[B](right: Rep[B]) = semicolon(left, right)
  }
}

trait BlocksSeq extends Blocks { self: ScalanSeq =>
  def semicolon[A,B](left: Rep[A], right: Rep[B]): Rep[B] = right
}

trait BlocksExp extends Blocks { self: ScalanExp =>
  case class Semicolon[A,B](left: Exp[A], right: Exp[B])(implicit selfType: Elem[B]) extends BaseDef[B] {
    override def mirror(t: Transformer) = Semicolon(t(left), t(right))
  }

  def semicolon[A,B](left: Rep[A], right: Rep[B]): Rep[B] = { 
    implicit val eR = right.elem
    Semicolon(left, right)
  }
}
