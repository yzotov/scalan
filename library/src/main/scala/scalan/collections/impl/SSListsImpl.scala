package scalan.collections

import scala.reflect.ClassTag
import scalan._
import scalan.common.Default
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
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

  implicit val containerSSList: Cont[SSList] with Functor[SSList] = new Container[SSList] with Functor[SSList] {
    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[SSList[A]]
    def lift[A](implicit evA: Elem[A]) = element[SSList[A]]
    def map[A:Elem,B:Elem](xs: Rep[SSList[A]])(f: Rep[A] => Rep[B]) = xs.map(fun(f))
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
    lazy val parent: Option[Elem[_]] = None
    lazy val entityDef: STraitOrClassDef = {
      val module = getModules("SSLists")
      module.entities.find(_.name == "SSList").get
    }
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA))
    }
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
  implicit def proxySSListCompanion(p: Rep[SSListCompanion]): SSListCompanion =
    proxyOps[SSListCompanion](p)

  // default wrapper implementation
  abstract class SSListImpl[A](val wrappedValueOfBaseType: Rep[List[A]])(implicit val eA: Elem[A]) extends SSList[A] {
    def length: Rep[Int] =
      methodCallEx[Int](self,
        this.getClass.getMethod("length"),
        List())

    def toArray: Rep[Array[A]] =
      methodCallEx[Array[A]](self,
        this.getClass.getMethod("toArray"),
        List())

    def map[B:Elem](f: Rep[A => B]): Rep[SSList[B]] =
      methodCallEx[SSList[B]](self,
        this.getClass.getMethod("map", classOf[AnyRef], classOf[Elem[B]]),
        List(f.asInstanceOf[AnyRef], element[B]))
  }
  trait SSListImplCompanion
  // elem for concrete class
  class SSListImplElem[A](val iso: Iso[SSListImplData[A], SSListImpl[A]])(implicit eA: Elem[A])
    extends SSListElem[A, SSListImpl[A]]
    with ConcreteElem1[A, SSListImplData[A], SSListImpl[A], SSList] {
    override lazy val parent: Option[Elem[_]] = Some(sSListElement(element[A]))
    override lazy val entityDef = {
      val module = getModules("SSLists")
      module.concreteSClasses.find(_.name == "SSListImpl").get
    }
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA))
    }
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
  abstract class SSListImplCompanionAbs extends CompanionBase[SSListImplCompanionAbs] {
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

  registerModule(scalan.meta.ScalanCodegen.loadModule(SSLists_Module.dump))
}

// Seq -----------------------------------
trait SSListsSeq extends SSListsDsl with scalan.ScalanSeq {
  self: ScalanCommunityDslSeq =>
  lazy val SSList: Rep[SSListCompanionAbs] = new SSListCompanionAbs with UserTypeSeq[SSListCompanionAbs] {
    lazy val selfType = element[SSListCompanionAbs]
    override def empty[A:Elem]: Rep[SSList[A]] =
      SSListImpl(List.empty[A])

    override def apply[A:Elem](xs: Rep[Array[A]]): Rep[SSList[A]] =
      SSListImpl(List.apply[A](xs: _*))
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
    override def length: Rep[Int] =
      wrappedValueOfBaseType.length

    override def toArray: Rep[Array[A]] =
      wrappedValueOfBaseType.toArray
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

    def apply[A:Elem](xs: Rep[Array[A]]): Rep[SSList[A]] =
      methodCallEx[SSList[A]](self,
        this.getClass.getMethod("apply", classOf[AnyRef], classOf[Elem[A]]),
        List(xs.asInstanceOf[AnyRef], element[A]))
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

    object length {
      def unapply(d: Def[_]): Option[Rep[SSList[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SSListElem[_, _]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[SSList[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[SSList[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object toArray {
      def unapply(d: Def[_]): Option[Rep[SSList[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SSListElem[_, _]] && method.getName == "toArray" =>
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

    object apply {
      def unapply(d: Def[_]): Option[Rep[Array[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(xs, _*), _) if receiver.elem == SSListCompanionElem && method.getName == "apply" =>
          Some(xs).asInstanceOf[Option[Rep[Array[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Array[A]] forSome {type A}] = exp match {
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

object SSLists_Module {
  val packageName = "scalan.collections"
  val name = "SSLists"
  val dump = "H4sIAAAAAAAAALVWT4wTVRh/nW7bbbuygCuKYXXZVBGF7UJUYjhg9x9Z6VKyA6iVrHmdeV0G5l9nXnGqEZONJgZuSkwkMchBT9w8aDx4MDExkphoiJgoBw96EDCGqMQo6vfem5l2yk5ZTdzDbN+8N9+f3+/3fd87dxWlXAfd7ypYx+aYQSgek/nvkksL8rRJNdqas9SmTqZIfWndB8qcOeFKaLCK0oexO+XqVZQVP6Y9O/wtk0YZZbGpEJdajkvRxjL3UFQsXScK1SyzqBlGk+KaToplzaU7y6ivZqmtBjqOEmW0WrFMxSGUyJM6dl3i+u/7CYtIC9dZvm5V7LYPs8iyKHZksd/BGoXwwcdqcX6e2HLLtMyWQdEqP7SKzcKCMxnNsC2HBi4yYO6wpQbLPhPDC7S2fAQfw0VwsViUqaOZi/Bl3sbKUbxI9sIRdrwPAnaJXt/fsvk6WUY5lzQAoFnD1vkbz0YIAQPbeRBjbXzGQnzGGD4FmTga1rXnMdvc51heC4m/RBIhzwYTW25hIrBApk21cOKQ8sx1OW9I7GOPhZLhGabB0L0xauBUAI6fzr/mXtt9doeEclWU09xSzaUOVmgn5T5aeWyaFuUxhwBiZxHYGo1ji3spwZkuSWQVy7CxCZZ8KAeAJ11TNMoOs3cDPjsx0GeoTYKjCc9OhPmOxOTLdTOJdX3f5fVb77sy/ZSEpKiLLJiUQfhOYJSitCwzsH3j7DlIUaLEEWaPrNd+Zno4D2HYdPkn9ZNxdEgKwfN9rYwvMJFyv/4qf2HzLgn1V7m6Z3S8WAX83GmdGBVn0jJpFfVbx4gjdjLHsM5+LctfRiV13NSpj2onHEmAg6KR2Dq0CcNqJ9d8IgAgL2S71zJJYWZf4Tf5s1PnmCodNCB2RGH+pe248c2qOuWCpWjdcw62baIexHqTVOoT2CWM6gDwJFR4lIK+FbPic8Mew/zoHR2f3ZUIQuf7FEmkFLpgcN7SBUU5IZGgBwyHRA7HCZELd918eUi/uusjCaWeQKk68OOWUapmNU01qAjompR4dCJ4l4jyAxWAHWwEAhL9YwTxIMI4h26K+JZCCxrsr9Vx6Zf1F89IKAt6qmnUwHZhfIVt4X8sdRQlJc9OPsn1IyJKs8dosP1vKrgDnK29wIEi48UYpq0Vtjz2w9SpPbylDLbh4Mf8rDqLnaLbWJlizSROkKjo2CVIaKZpKgBvuBFtMTlRR7JlkDWj17SFsycpbyYJLzrHKrUjMDh28u82cNuPdCE3MO1NBtRsi275qC3f59oFA4kMipOTnRwL+dnsuSZcb2tbeDSq0QxFGZ8lSMEHvj34BIIzkPpoDCmyrxcQ7fHrb+198PP3vudM5JjyoBOZ4fhvy8zr6ihrhT3Iw2iacKmAsd6RPbQDJsowkgdiI2mwRkAM8MkVsIQOnpva/rbBKRoknhD+XMclpKP1x00PcRyyu/R0o/H++EOr+Kzu6vgwumf9uuKLCgwAR1PJsvWZBzHKPrzdJds1BnJskugWVmeDxhSpxzJKw3Je9GpxAWoTG+K1ISazCd1Sjh7Y+mO9MfTwDTGQNUA/CAnKh1I0HDeC/PkTVEacE8DHpXfPDX1R27N4QtSKwr7ZzS9ad4p6cppwlzTI2ITlEfUAxEDffXH38JXzp/0RlS6wwApR5YpmvxDOv4DIjT2JZNht/njTGwvND0/G3wN6iwFsJL/9+/zrjpuUUGYlF4H/Mv7Zz/VRTmFIJurLDGYH3RM/71hHuzD75u2Dw89+x1lOq5YB3Y/bh7HnQMMKnHX16HA50atl99JYybb11sv1zWe+PP3nSxJLM8XwDiBI1ps8jloZSbSLhfbzhcibeGezKlT+prXv/PFz9mJF6m457J8dCT7+mgBNDQo0f0kj5PHfXxG1Af3D8WNN0e5WthR7jwKiYHYvfxNYuukGw8+82pa3SHp75H4hNvxLUtAiIxT5tNwslJ5zOY7kfwAjybe52g4AAA=="
}
}

