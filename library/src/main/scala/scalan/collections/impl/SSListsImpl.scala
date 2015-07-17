package scalan.collections
package impl

import scala.reflect.ClassTag
import scalan._
import scalan.common.Default
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}

// Abs -----------------------------------
trait SSListsAbs extends SSLists with scalan.Scalan {
  self: ScalanCommunityDsl =>

  // single proxy for each type family
  implicit def proxySSList[A](p: Rep[SSList[A]]): SSList[A] = {
    proxyOps[SSList[A]](p)(scala.reflect.classTag[SSList[A]])
  }

  // TypeWrapper proxy
  //implicit def proxyList[A:Elem](p: Rep[List[A]]): SSList[A] =
  //  proxyOps[SSList[A]](p.asRep[SSList[A]])

  implicit def unwrapValueOfSSList[A](w: Rep[SSList[A]]): Rep[List[A]] = w.wrappedValueOfBaseType

  implicit def listElement[A:Elem]: Elem[List[A]]

  implicit def castSSListElement[A](elem: Elem[SSList[A]]): SSListElem[A, SSList[A]] = elem.asInstanceOf[SSListElem[A, SSList[A]]]

  implicit val containerList: Cont[List] = new Container[List] {
    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[List[A]]
    def lift[A](implicit evA: Elem[A]) = element[List[A]]
  }

  implicit val containerSSList: Cont[SSList] = new Container[SSList] {
    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[SSList[A]]
    def lift[A](implicit evA: Elem[A]) = element[SSList[A]]
  }
  case class SSListIso[A,B](iso: Iso[A,B]) extends Iso1[A, B, SSList](iso) {
    implicit val eA = iso.eFrom
    implicit val eB = iso.eTo
    def from(x: Rep[SSList[B]]) = x.map(iso.from _)
    def to(x: Rep[SSList[A]]) = x.map(iso.to _)
    lazy val defaultRepTo = SSList.empty[B]
  }

  // familyElem
  abstract class SSListElem[A, To <: SSList[A]](implicit val eA: Elem[A])
    extends WrapperElem1[A, To, List, SSList]()(eA, container[List], container[SSList]) {
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[SSList[A]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Reifiable[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[SSList[A]] => convertSSList(x) }
      tryConvert(element[SSList[A]], this, x, conv)
    }

    def convertSSList(x : Rep[SSList[A]]): Rep[To] = {
      assert(x.selfType1 match { case _: SSListElem[_, _] => true; case _ => false })
      x.asRep[To]
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def sSListElement[A](implicit eA: Elem[A]): Elem[SSList[A]] =
    new SSListElem[A, SSList[A]] {
      lazy val eTo = element[SSListImpl[A]]
    }

  implicit case object SSListCompanionElem extends CompanionElem[SSListCompanionAbs] {
    lazy val tag = weakTypeTag[SSListCompanionAbs]
    protected def getDefaultRep = SSList
  }

  abstract class SSListCompanionAbs extends CompanionBase[SSListCompanionAbs] with SSListCompanion {
    override def toString = "SSList"
  }
  def SSList: Rep[SSListCompanionAbs]
  implicit def proxySSListCompanion(p: Rep[SSListCompanion]): SSListCompanion = {
    proxyOps[SSListCompanion](p)
  }

  // default wrapper implementation
  abstract class SSListImpl[A](val wrappedValueOfBaseType: Rep[List[A]])(implicit val eA: Elem[A]) extends SSList[A] {
  }
  trait SSListImplCompanion
  // elem for concrete class
  class SSListImplElem[A](val iso: Iso[SSListImplData[A], SSListImpl[A]])(implicit eA: Elem[A])
    extends SSListElem[A, SSListImpl[A]]
    with ConcreteElem1[A, SSListImplData[A], SSListImpl[A], SSList] {
    lazy val eTo = this
    override def convertSSList(x: Rep[SSList[A]]) = SSListImpl(x.wrappedValueOfBaseType)
    override def getDefaultRep = super[ConcreteElem1].getDefaultRep
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[SSListImpl[A]]
    }
  }

  // state representation type
  type SSListImplData[A] = List[A]

  // 3) Iso for concrete class
  class SSListImplIso[A](implicit eA: Elem[A])
    extends Iso[SSListImplData[A], SSListImpl[A]] {
    override def from(p: Rep[SSListImpl[A]]) =
      p.wrappedValueOfBaseType
    override def to(p: Rep[List[A]]) = {
      val wrappedValueOfBaseType = p
      SSListImpl(wrappedValueOfBaseType)
    }
    lazy val defaultRepTo: Rep[SSListImpl[A]] = SSListImpl(DefaultOfList[A].value)
    lazy val eTo = new SSListImplElem[A](this)
  }
  // 4) constructor and deconstructor
  abstract class SSListImplCompanionAbs extends CompanionBase[SSListImplCompanionAbs] with SSListImplCompanion {
    override def toString = "SSListImpl"

    def apply[A](wrappedValueOfBaseType: Rep[List[A]])(implicit eA: Elem[A]): Rep[SSListImpl[A]] =
      mkSSListImpl(wrappedValueOfBaseType)
  }
  object SSListImplMatcher {
    def unapply[A](p: Rep[SSList[A]]) = unmkSSListImpl(p)
  }
  def SSListImpl: Rep[SSListImplCompanionAbs]
  implicit def proxySSListImplCompanion(p: Rep[SSListImplCompanionAbs]): SSListImplCompanionAbs = {
    proxyOps[SSListImplCompanionAbs](p)
  }

  implicit case object SSListImplCompanionElem extends CompanionElem[SSListImplCompanionAbs] {
    lazy val tag = weakTypeTag[SSListImplCompanionAbs]
    protected def getDefaultRep = SSListImpl
  }

  implicit def proxySSListImpl[A](p: Rep[SSListImpl[A]]): SSListImpl[A] =
    proxyOps[SSListImpl[A]](p)

  implicit class ExtendedSSListImpl[A](p: Rep[SSListImpl[A]])(implicit eA: Elem[A]) {
    def toData: Rep[SSListImplData[A]] = isoSSListImpl(eA).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoSSListImpl[A](implicit eA: Elem[A]): Iso[SSListImplData[A], SSListImpl[A]] =
    new SSListImplIso[A]

  // 6) smart constructor and deconstructor
  def mkSSListImpl[A](wrappedValueOfBaseType: Rep[List[A]])(implicit eA: Elem[A]): Rep[SSListImpl[A]]
  def unmkSSListImpl[A](p: Rep[SSList[A]]): Option[(Rep[List[A]])]
}

// Seq -----------------------------------
trait SSListsSeq extends SSListsDsl with scalan.ScalanSeq {
  self: ScalanCommunityDslSeq =>
  lazy val SSList: Rep[SSListCompanionAbs] = new SSListCompanionAbs with UserTypeSeq[SSListCompanionAbs] {
    lazy val selfType = element[SSListCompanionAbs]
    override def empty[A:Elem]: Rep[SSList[A]] =
      SSListImpl(List.empty[A])
  }

    // override proxy if we deal with TypeWrapper
  //override def proxyList[A:Elem](p: Rep[List[A]]): SSList[A] =
  //  proxyOpsEx[List[A],SSList[A], SeqSSListImpl[A]](p, bt => SeqSSListImpl(bt))

    implicit def listElement[A:Elem]: Elem[List[A]] =
      new SeqBaseElemEx1[A, SSList[A], List](
           element[SSList[A]])(element[A], container[List], DefaultOfList[A])

  case class SeqSSListImpl[A]
      (override val wrappedValueOfBaseType: Rep[List[A]])
      (implicit eA: Elem[A])
    extends SSListImpl[A](wrappedValueOfBaseType)
       with SeqSSList[A] with UserTypeSeq[SSListImpl[A]] {
    lazy val selfType = element[SSListImpl[A]]
  }
  lazy val SSListImpl = new SSListImplCompanionAbs with UserTypeSeq[SSListImplCompanionAbs] {
    lazy val selfType = element[SSListImplCompanionAbs]
  }

  def mkSSListImpl[A]
      (wrappedValueOfBaseType: Rep[List[A]])(implicit eA: Elem[A]): Rep[SSListImpl[A]] =
      new SeqSSListImpl[A](wrappedValueOfBaseType)
  def unmkSSListImpl[A](p: Rep[SSList[A]]) = p match {
    case p: SSListImpl[A] @unchecked =>
      Some((p.wrappedValueOfBaseType))
    case _ => None
  }

  implicit def wrapListToSSList[A:Elem](v: List[A]): SSList[A] = SSListImpl(v)
}

// Exp -----------------------------------
trait SSListsExp extends SSListsDsl with scalan.ScalanExp {
  self: ScalanCommunityDslExp =>
  lazy val SSList: Rep[SSListCompanionAbs] = new SSListCompanionAbs with UserTypeDef[SSListCompanionAbs] {
    lazy val selfType = element[SSListCompanionAbs]
    override def mirror(t: Transformer) = this

    def empty[A:Elem]: Rep[SSList[A]] =
      methodCallEx[SSList[A]](self,
        this.getClass.getMethod("empty", classOf[Elem[A]]),
        List(element[A]))
  }

  case class ViewSSList[A, B](source: Rep[SSList[A]])(iso: Iso1[A, B, SSList])
    extends View1[A, B, SSList](iso) {
    def copy(source: Rep[SSList[A]]) = ViewSSList(source)(iso)
    override def toString = s"ViewSSList[${innerIso.eTo.name}]($source)"
    override def equals(other: Any) = other match {
      case v: ViewSSList[_, _] => source == v.source && innerIso.eTo == v.innerIso.eTo
      case _ => false
    }
  }

  implicit def listElement[A:Elem]: Elem[List[A]] =
      new ExpBaseElemEx1[A, SSList[A], List](
           element[SSList[A]])(element[A], container[List], DefaultOfList[A])

  case class ExpSSListImpl[A]
      (override val wrappedValueOfBaseType: Rep[List[A]])
      (implicit eA: Elem[A])
    extends SSListImpl[A](wrappedValueOfBaseType) with UserTypeDef[SSListImpl[A]] {
    lazy val selfType = element[SSListImpl[A]]
    override def mirror(t: Transformer) = ExpSSListImpl[A](t(wrappedValueOfBaseType))
  }

  lazy val SSListImpl: Rep[SSListImplCompanionAbs] = new SSListImplCompanionAbs with UserTypeDef[SSListImplCompanionAbs] {
    lazy val selfType = element[SSListImplCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object SSListImplMethods {
  }

  def mkSSListImpl[A]
    (wrappedValueOfBaseType: Rep[List[A]])(implicit eA: Elem[A]): Rep[SSListImpl[A]] =
    new ExpSSListImpl[A](wrappedValueOfBaseType)
  def unmkSSListImpl[A](p: Rep[SSList[A]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SSListImplElem[A] @unchecked =>
      Some((p.asRep[SSListImpl[A]].wrappedValueOfBaseType))
    case _ =>
      None
  }

  object SSListMethods {
    object wrappedValueOfBaseType {
      def unapply(d: Def[_]): Option[Rep[SSList[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SSListElem[_, _]] && method.getName == "wrappedValueOfBaseType" =>
          Some(receiver).asInstanceOf[Option[Rep[SSList[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[SSList[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object map {
      def unapply(d: Def[_]): Option[(Rep[SSList[A]], Rep[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[SSListElem[_, _]] && method.getName == "map" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[SSList[A]], Rep[A => B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SSList[A]], Rep[A => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object SSListCompanionMethods {
    object empty {
      def unapply(d: Def[_]): Option[Unit forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem == SSListCompanionElem && method.getName == "empty" =>
          Some(()).asInstanceOf[Option[Unit forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object UserTypeSSList {
    def unapply(s: Exp[_]): Option[Iso[_, _]] = {
      s.elem match {
        case e: SSListElem[a,to] => e.eItem match {
          case UnpackableElem(iso) => Some(iso)
          case _ => None
        }
        case _ => None
      }
    }
  }

  override def unapplyViews[T](s: Exp[T]): Option[Unpacked[T]] = (s match {
    case Def(view: ViewSSList[_, _]) =>
      Some((view.source, view.iso))
    case UserTypeSSList(iso: Iso[a, b]) =>
      val newIso = SSListIso(iso)
      val repr = reifyObject(UnpackView(s.asRep[SSList[b]])(newIso))
      Some((repr, newIso))
    case _ =>
      super.unapplyViews(s)
  }).asInstanceOf[Option[Unpacked[T]]]

  override def rewriteDef[T](d: Def[T]) = d match {
    case SSListMethods.map(xs, Def(IdentityLambda())) => xs

    case view1@ViewSSList(Def(view2@ViewSSList(arr))) =>
      val compIso = composeIso(view1.innerIso, view2.innerIso)
      implicit val eAB = compIso.eTo
      ViewSSList(arr)(SSListIso(compIso))

    // Rule: W(a).m(args) ==> iso.to(a.m(unwrap(args)))
    case mc @ MethodCall(Def(wrapper: ExpSSListImpl[_]), m, args, neverInvoke) if !isValueAccessor(m) =>
      val resultElem = mc.selfType
      val wrapperIso = getIsoByElem(resultElem)
      wrapperIso match {
        case iso: Iso[base,ext] =>
          val eRes = iso.eFrom
          val newCall = unwrapMethodCall(mc, wrapper.wrappedValueOfBaseType, eRes)
          iso.to(newCall)
      }

    case SSListMethods.map(xs, f) => (xs, f) match {
      case (xs: RepSSList[a] @unchecked, LambdaResultHasViews(f, iso: Iso[b, c])) =>
        val f1 = f.asRep[a => c]
        implicit val eA = xs.elem.eItem
        implicit val eB = iso.eFrom
        val s = xs.map(fun { x =>
          val tmp = f1(x)
          iso.from(tmp)
        })
        val res = ViewSSList(s)(SSListIso(iso))
        res
      case (HasViews(source, contIso: SSListIso[a, b]), f: Rep[Function1[_, c] @unchecked]) =>
        val f1 = f.asRep[b => c]
        val iso = contIso.iso
        implicit val eA = iso.eFrom
        implicit val eB = iso.eTo
        implicit val eC = f1.elem.eRange
        source.asRep[SSList[a]].map(fun { x => f1(iso.to(x)) })
      case _ =>
        super.rewriteDef(d)
    }

    case _ => super.rewriteDef(d)
  }
}
