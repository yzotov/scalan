package scalan.graphs
package impl

import scala.annotation.unchecked.uncheckedVariance
import scalan.common.Default
import scalan._
import scalan.collection.{CollectionsDslExp, CollectionsDslSeq, CollectionsDsl}
import scala.reflect.runtime.universe._
import scala.reflect._
import scalan.common.Default

// Abs -----------------------------------
trait EdgesAbs extends ScalanCommunityDsl with Edges {
  self: GraphsDsl =>
  // single proxy for each type family
  implicit def proxyEdge[V, E](p: Rep[Edge[V, E]]): Edge[V, E] = {
    implicit val tag = weakTypeTag[Edge[V, E]]
    proxyOps[Edge[V, E]](p)(TagImplicits.typeTagToClassTag[Edge[V, E]])
  }

  abstract class EdgeElem[V, E, From, To <: Edge[V, E]](iso: Iso[From, To])(implicit eV: Elem[V], eE: Elem[E])
    extends ViewElem[From, To](iso) {
    override def convert(x: Rep[Reifiable[_]]) = convertEdge(x.asRep[Edge[V, E]])
    def convertEdge(x : Rep[Edge[V, E]]): Rep[To]
  }
  trait EdgeCompanionElem extends CompanionElem[EdgeCompanionAbs]
  implicit lazy val EdgeCompanionElem: EdgeCompanionElem = new EdgeCompanionElem {
    lazy val tag = weakTypeTag[EdgeCompanionAbs]
    protected def getDefaultRep = Edge
  }

  abstract class EdgeCompanionAbs extends CompanionBase[EdgeCompanionAbs] with EdgeCompanion {
    override def toString = "Edge"
  }
  def Edge: Rep[EdgeCompanionAbs]
  implicit def proxyEdgeCompanion(p: Rep[EdgeCompanion]): EdgeCompanion = {
    proxyOps[EdgeCompanion](p)
  }

  // elem for concrete class
  class AdjEdgeElem[V, E](iso: Iso[AdjEdgeData[V, E], AdjEdge[V, E]])(implicit val eV: Elem[V], val eE: Elem[E])
    extends EdgeElem[V, E, AdjEdgeData[V, E], AdjEdge[V, E]](iso) {
    def convertEdge(x: Rep[Edge[V, E]]) = AdjEdge(x.fromId, x.outIndex, x.graph)
  }

  // state representation type
  type AdjEdgeData[V, E] = (Int, (Int, Graph[V,E]))

  // 3) Iso for concrete class
  class AdjEdgeIso[V, E](implicit eV: Elem[V], eE: Elem[E])
    extends Iso[AdjEdgeData[V, E], AdjEdge[V, E]] {
    override def from(p: Rep[AdjEdge[V, E]]) =
      unmkAdjEdge(p) match {
        case Some((fromId, outIndex, graph)) => Pair(fromId, Pair(outIndex, graph))
        case None => !!!
      }
    override def to(p: Rep[(Int, (Int, Graph[V,E]))]) = {
      val Pair(fromId, Pair(outIndex, graph)) = p
      AdjEdge(fromId, outIndex, graph)
    }
    lazy val tag = {
      weakTypeTag[AdjEdge[V, E]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[AdjEdge[V, E]]](AdjEdge(0, 0, element[Graph[V,E]].defaultRepValue))
    lazy val eTo = new AdjEdgeElem[V, E](this)
  }
  // 4) constructor and deconstructor
  abstract class AdjEdgeCompanionAbs extends CompanionBase[AdjEdgeCompanionAbs] with AdjEdgeCompanion {
    override def toString = "AdjEdge"
    def apply[V, E](p: Rep[AdjEdgeData[V, E]])(implicit eV: Elem[V], eE: Elem[E]): Rep[AdjEdge[V, E]] =
      isoAdjEdge(eV, eE).to(p)
    def apply[V, E](fromId: Rep[Int], outIndex: Rep[Int], graph: PG[V,E])(implicit eV: Elem[V], eE: Elem[E]): Rep[AdjEdge[V, E]] =
      mkAdjEdge(fromId, outIndex, graph)
    def unapply[V:Elem, E:Elem](p: Rep[AdjEdge[V, E]]) = unmkAdjEdge(p)
  }
  def AdjEdge: Rep[AdjEdgeCompanionAbs]
  implicit def proxyAdjEdgeCompanion(p: Rep[AdjEdgeCompanionAbs]): AdjEdgeCompanionAbs = {
    proxyOps[AdjEdgeCompanionAbs](p)
  }

  class AdjEdgeCompanionElem extends CompanionElem[AdjEdgeCompanionAbs] {
    lazy val tag = weakTypeTag[AdjEdgeCompanionAbs]
    protected def getDefaultRep = AdjEdge
  }
  implicit lazy val AdjEdgeCompanionElem: AdjEdgeCompanionElem = new AdjEdgeCompanionElem

  implicit def proxyAdjEdge[V, E](p: Rep[AdjEdge[V, E]]): AdjEdge[V, E] =
    proxyOps[AdjEdge[V, E]](p)

  implicit class ExtendedAdjEdge[V, E](p: Rep[AdjEdge[V, E]])(implicit eV: Elem[V], eE: Elem[E]) {
    def toData: Rep[AdjEdgeData[V, E]] = isoAdjEdge(eV, eE).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoAdjEdge[V, E](implicit eV: Elem[V], eE: Elem[E]): Iso[AdjEdgeData[V, E], AdjEdge[V, E]] =
    new AdjEdgeIso[V, E]

  // 6) smart constructor and deconstructor
  def mkAdjEdge[V, E](fromId: Rep[Int], outIndex: Rep[Int], graph: PG[V,E])(implicit eV: Elem[V], eE: Elem[E]): Rep[AdjEdge[V, E]]
  def unmkAdjEdge[V:Elem, E:Elem](p: Rep[AdjEdge[V, E]]): Option[(Rep[Int], Rep[Int], Rep[Graph[V,E]])]

  // elem for concrete class
  class IncEdgeElem[V, E](iso: Iso[IncEdgeData[V, E], IncEdge[V, E]])(implicit val eV: Elem[V], val eE: Elem[E])
    extends EdgeElem[V, E, IncEdgeData[V, E], IncEdge[V, E]](iso) {
    def convertEdge(x: Rep[Edge[V, E]]) = IncEdge(x.fromId, x.toId, x.graph)
  }

  // state representation type
  type IncEdgeData[V, E] = (Int, (Int, Graph[V,E]))

  // 3) Iso for concrete class
  class IncEdgeIso[V, E](implicit eV: Elem[V], eE: Elem[E])
    extends Iso[IncEdgeData[V, E], IncEdge[V, E]] {
    override def from(p: Rep[IncEdge[V, E]]) =
      unmkIncEdge(p) match {
        case Some((fromId, toId, graph)) => Pair(fromId, Pair(toId, graph))
        case None => !!!
      }
    override def to(p: Rep[(Int, (Int, Graph[V,E]))]) = {
      val Pair(fromId, Pair(toId, graph)) = p
      IncEdge(fromId, toId, graph)
    }
    lazy val tag = {
      weakTypeTag[IncEdge[V, E]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[IncEdge[V, E]]](IncEdge(0, 0, element[Graph[V,E]].defaultRepValue))
    lazy val eTo = new IncEdgeElem[V, E](this)
  }
  // 4) constructor and deconstructor
  abstract class IncEdgeCompanionAbs extends CompanionBase[IncEdgeCompanionAbs] with IncEdgeCompanion {
    override def toString = "IncEdge"
    def apply[V, E](p: Rep[IncEdgeData[V, E]])(implicit eV: Elem[V], eE: Elem[E]): Rep[IncEdge[V, E]] =
      isoIncEdge(eV, eE).to(p)
    def apply[V, E](fromId: Rep[Int], toId: Rep[Int], graph: PG[V,E])(implicit eV: Elem[V], eE: Elem[E]): Rep[IncEdge[V, E]] =
      mkIncEdge(fromId, toId, graph)
    def unapply[V:Elem, E:Elem](p: Rep[IncEdge[V, E]]) = unmkIncEdge(p)
  }
  def IncEdge: Rep[IncEdgeCompanionAbs]
  implicit def proxyIncEdgeCompanion(p: Rep[IncEdgeCompanionAbs]): IncEdgeCompanionAbs = {
    proxyOps[IncEdgeCompanionAbs](p)
  }

  class IncEdgeCompanionElem extends CompanionElem[IncEdgeCompanionAbs] {
    lazy val tag = weakTypeTag[IncEdgeCompanionAbs]
    protected def getDefaultRep = IncEdge
  }
  implicit lazy val IncEdgeCompanionElem: IncEdgeCompanionElem = new IncEdgeCompanionElem

  implicit def proxyIncEdge[V, E](p: Rep[IncEdge[V, E]]): IncEdge[V, E] =
    proxyOps[IncEdge[V, E]](p)

  implicit class ExtendedIncEdge[V, E](p: Rep[IncEdge[V, E]])(implicit eV: Elem[V], eE: Elem[E]) {
    def toData: Rep[IncEdgeData[V, E]] = isoIncEdge(eV, eE).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoIncEdge[V, E](implicit eV: Elem[V], eE: Elem[E]): Iso[IncEdgeData[V, E], IncEdge[V, E]] =
    new IncEdgeIso[V, E]

  // 6) smart constructor and deconstructor
  def mkIncEdge[V, E](fromId: Rep[Int], toId: Rep[Int], graph: PG[V,E])(implicit eV: Elem[V], eE: Elem[E]): Rep[IncEdge[V, E]]
  def unmkIncEdge[V:Elem, E:Elem](p: Rep[IncEdge[V, E]]): Option[(Rep[Int], Rep[Int], Rep[Graph[V,E]])]
}

// Seq -----------------------------------
trait EdgesSeq extends ScalanCommunityDslSeq {
  self: GraphsDslSeq =>
  lazy val Edge: Rep[EdgeCompanionAbs] = new EdgeCompanionAbs with UserTypeSeq[EdgeCompanionAbs, EdgeCompanionAbs] {
    lazy val selfType = element[EdgeCompanionAbs]
  }

  case class SeqAdjEdge[V, E]
      (override val fromId: Rep[Int], override val outIndex: Rep[Int], override val graph: PG[V,E])
      (implicit eV: Elem[V], eE: Elem[E])
    extends AdjEdge[V, E](fromId, outIndex, graph)
        with UserTypeSeq[Edge[V,E], AdjEdge[V, E]] {
    lazy val selfType = element[AdjEdge[V, E]].asInstanceOf[Elem[Edge[V,E]]]
  }
  lazy val AdjEdge = new AdjEdgeCompanionAbs with UserTypeSeq[AdjEdgeCompanionAbs, AdjEdgeCompanionAbs] {
    lazy val selfType = element[AdjEdgeCompanionAbs]
  }

  def mkAdjEdge[V, E]
      (fromId: Rep[Int], outIndex: Rep[Int], graph: PG[V,E])(implicit eV: Elem[V], eE: Elem[E]): Rep[AdjEdge[V, E]] =
      new SeqAdjEdge[V, E](fromId, outIndex, graph)
  def unmkAdjEdge[V:Elem, E:Elem](p: Rep[AdjEdge[V, E]]) =
    Some((p.fromId, p.outIndex, p.graph))

  case class SeqIncEdge[V, E]
      (override val fromId: Rep[Int], override val toId: Rep[Int], override val graph: PG[V,E])
      (implicit eV: Elem[V], eE: Elem[E])
    extends IncEdge[V, E](fromId, toId, graph)
        with UserTypeSeq[Edge[V,E], IncEdge[V, E]] {
    lazy val selfType = element[IncEdge[V, E]].asInstanceOf[Elem[Edge[V,E]]]
  }
  lazy val IncEdge = new IncEdgeCompanionAbs with UserTypeSeq[IncEdgeCompanionAbs, IncEdgeCompanionAbs] {
    lazy val selfType = element[IncEdgeCompanionAbs]
  }

  def mkIncEdge[V, E]
      (fromId: Rep[Int], toId: Rep[Int], graph: PG[V,E])(implicit eV: Elem[V], eE: Elem[E]): Rep[IncEdge[V, E]] =
      new SeqIncEdge[V, E](fromId, toId, graph)
  def unmkIncEdge[V:Elem, E:Elem](p: Rep[IncEdge[V, E]]) =
    Some((p.fromId, p.toId, p.graph))
}

// Exp -----------------------------------
trait EdgesExp extends ScalanCommunityDslExp {
  self: GraphsDslExp =>
  lazy val Edge: Rep[EdgeCompanionAbs] = new EdgeCompanionAbs with UserTypeDef[EdgeCompanionAbs, EdgeCompanionAbs] {
    lazy val selfType = element[EdgeCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpAdjEdge[V, E]
      (override val fromId: Rep[Int], override val outIndex: Rep[Int], override val graph: PG[V,E])
      (implicit eV: Elem[V], eE: Elem[E])
    extends AdjEdge[V, E](fromId, outIndex, graph) with UserTypeDef[Edge[V,E], AdjEdge[V, E]] {
    lazy val selfType = element[AdjEdge[V, E]].asInstanceOf[Elem[Edge[V,E]]]
    override def mirror(t: Transformer) = ExpAdjEdge[V, E](t(fromId), t(outIndex), t(graph))
  }

  lazy val AdjEdge: Rep[AdjEdgeCompanionAbs] = new AdjEdgeCompanionAbs with UserTypeDef[AdjEdgeCompanionAbs, AdjEdgeCompanionAbs] {
    lazy val selfType = element[AdjEdgeCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object AdjEdgeMethods {
    object indexOfTarget {
      def unapply(d: Def[_]): Option[Rep[AdjEdge[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AdjEdgeElem[_, _]] && method.getName == "indexOfTarget" =>
          Some(receiver).asInstanceOf[Option[Rep[AdjEdge[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AdjEdge[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object toId {
      def unapply(d: Def[_]): Option[Rep[AdjEdge[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AdjEdgeElem[_, _]] && method.getName == "toId" =>
          Some(receiver).asInstanceOf[Option[Rep[AdjEdge[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AdjEdge[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromNode {
      def unapply(d: Def[_]): Option[Rep[AdjEdge[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AdjEdgeElem[_, _]] && method.getName == "fromNode" =>
          Some(receiver).asInstanceOf[Option[Rep[AdjEdge[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AdjEdge[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object toNode {
      def unapply(d: Def[_]): Option[Rep[AdjEdge[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AdjEdgeElem[_, _]] && method.getName == "toNode" =>
          Some(receiver).asInstanceOf[Option[Rep[AdjEdge[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AdjEdge[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object value {
      def unapply(d: Def[_]): Option[Rep[AdjEdge[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AdjEdgeElem[_, _]] && method.getName == "value" =>
          Some(receiver).asInstanceOf[Option[Rep[AdjEdge[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AdjEdge[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object AdjEdgeCompanionMethods {
    object defaultOf {
      def unapply(d: Def[_]): Option[Unit forSome {type T; type V}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AdjEdgeCompanionElem] && method.getName == "defaultOf" =>
          Some(()).asInstanceOf[Option[Unit forSome {type T; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit forSome {type T; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkAdjEdge[V, E]
    (fromId: Rep[Int], outIndex: Rep[Int], graph: PG[V,E])(implicit eV: Elem[V], eE: Elem[E]): Rep[AdjEdge[V, E]] =
    new ExpAdjEdge[V, E](fromId, outIndex, graph)
  def unmkAdjEdge[V:Elem, E:Elem](p: Rep[AdjEdge[V, E]]) =
    Some((p.fromId, p.outIndex, p.graph))

  case class ExpIncEdge[V, E]
      (override val fromId: Rep[Int], override val toId: Rep[Int], override val graph: PG[V,E])
      (implicit eV: Elem[V], eE: Elem[E])
    extends IncEdge[V, E](fromId, toId, graph) with UserTypeDef[Edge[V,E], IncEdge[V, E]] {
    lazy val selfType = element[IncEdge[V, E]].asInstanceOf[Elem[Edge[V,E]]]
    override def mirror(t: Transformer) = ExpIncEdge[V, E](t(fromId), t(toId), t(graph))
  }

  lazy val IncEdge: Rep[IncEdgeCompanionAbs] = new IncEdgeCompanionAbs with UserTypeDef[IncEdgeCompanionAbs, IncEdgeCompanionAbs] {
    lazy val selfType = element[IncEdgeCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object IncEdgeMethods {
    object indexOfTarget {
      def unapply(d: Def[_]): Option[Rep[IncEdge[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IncEdgeElem[_, _]] && method.getName == "indexOfTarget" =>
          Some(receiver).asInstanceOf[Option[Rep[IncEdge[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[IncEdge[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object outIndex {
      def unapply(d: Def[_]): Option[Rep[IncEdge[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IncEdgeElem[_, _]] && method.getName == "outIndex" =>
          Some(receiver).asInstanceOf[Option[Rep[IncEdge[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[IncEdge[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromNode {
      def unapply(d: Def[_]): Option[Rep[IncEdge[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IncEdgeElem[_, _]] && method.getName == "fromNode" =>
          Some(receiver).asInstanceOf[Option[Rep[IncEdge[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[IncEdge[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object toNode {
      def unapply(d: Def[_]): Option[Rep[IncEdge[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IncEdgeElem[_, _]] && method.getName == "toNode" =>
          Some(receiver).asInstanceOf[Option[Rep[IncEdge[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[IncEdge[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object value {
      def unapply(d: Def[_]): Option[Rep[IncEdge[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IncEdgeElem[_, _]] && method.getName == "value" =>
          Some(receiver).asInstanceOf[Option[Rep[IncEdge[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[IncEdge[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object IncEdgeCompanionMethods {
    object defaultOf {
      def unapply(d: Def[_]): Option[Unit forSome {type T; type V}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IncEdgeCompanionElem] && method.getName == "defaultOf" =>
          Some(()).asInstanceOf[Option[Unit forSome {type T; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit forSome {type T; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkIncEdge[V, E]
    (fromId: Rep[Int], toId: Rep[Int], graph: PG[V,E])(implicit eV: Elem[V], eE: Elem[E]): Rep[IncEdge[V, E]] =
    new ExpIncEdge[V, E](fromId, toId, graph)
  def unmkIncEdge[V:Elem, E:Elem](p: Rep[IncEdge[V, E]]) =
    Some((p.fromId, p.toId, p.graph))

  object EdgeMethods {
    object graph {
      def unapply(d: Def[_]): Option[Rep[Edge[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[EdgeElem[_, _, _, _]] && method.getName == "graph" =>
          Some(receiver).asInstanceOf[Option[Rep[Edge[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Edge[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object outIndex {
      def unapply(d: Def[_]): Option[Rep[Edge[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[EdgeElem[_, _, _, _]] && method.getName == "outIndex" =>
          Some(receiver).asInstanceOf[Option[Rep[Edge[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Edge[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromId {
      def unapply(d: Def[_]): Option[Rep[Edge[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[EdgeElem[_, _, _, _]] && method.getName == "fromId" =>
          Some(receiver).asInstanceOf[Option[Rep[Edge[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Edge[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object toId {
      def unapply(d: Def[_]): Option[Rep[Edge[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[EdgeElem[_, _, _, _]] && method.getName == "toId" =>
          Some(receiver).asInstanceOf[Option[Rep[Edge[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Edge[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromNode {
      def unapply(d: Def[_]): Option[Rep[Edge[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[EdgeElem[_, _, _, _]] && method.getName == "fromNode" =>
          Some(receiver).asInstanceOf[Option[Rep[Edge[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Edge[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object toNode {
      def unapply(d: Def[_]): Option[Rep[Edge[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[EdgeElem[_, _, _, _]] && method.getName == "toNode" =>
          Some(receiver).asInstanceOf[Option[Rep[Edge[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Edge[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object value {
      def unapply(d: Def[_]): Option[Rep[Edge[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[EdgeElem[_, _, _, _]] && method.getName == "value" =>
          Some(receiver).asInstanceOf[Option[Rep[Edge[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Edge[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object EdgeCompanionMethods {
    object defaultOf {
      def unapply(d: Def[_]): Option[Unit forSome {type T; type V}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[EdgeCompanionElem] && method.getName == "defaultOf" =>
          Some(()).asInstanceOf[Option[Unit forSome {type T; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit forSome {type T; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object MaxDoubleEdge {
      def unapply(d: Def[_]): Option[Unit] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[EdgeCompanionElem] && method.getName == "MaxDoubleEdge" =>
          Some(()).asInstanceOf[Option[Unit]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}