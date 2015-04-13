package scalan
package impl

import scala.reflect.runtime.universe._
import scala.reflect._
import scalan.common.Default

// Abs -----------------------------------
trait ConvertersAbs extends Converters  {
  self: Scalan =>
  // single proxy for each type family
  implicit def proxyConverter[T, R](p: Rep[Converter[T, R]]): Converter[T, R] = {
    proxyOps[Converter[T, R]](p)(classTag[Converter[T, R]])
  }

  class ConverterElem[T, R, To <: Converter[T, R]](implicit val eDom: Elem[T], val eRange: Elem[R])
    extends EntityElem[To] {
    override def isEntityType = true
    override def tag = {
      implicit val tagT = eDom.tag
      implicit val tagR = eRange.tag
      weakTypeTag[Converter[T, R]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Reifiable[_]]) = convertConverter(x.asRep[Converter[T, R]])
    def convertConverter(x : Rep[Converter[T, R]]): Rep[To] = {
      assert(x.selfType1.isInstanceOf[ConverterElem[_,_,_]])
      x.asRep[To]
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def converterElement[T, R](implicit eDom: Elem[T], eRange: Elem[R]) =
    new ConverterElem[T, R, Converter[T, R]]()(eDom, eRange)

  trait ConverterCompanionElem extends CompanionElem[ConverterCompanionAbs]
  implicit lazy val ConverterCompanionElem: ConverterCompanionElem = new ConverterCompanionElem {
    lazy val tag = weakTypeTag[ConverterCompanionAbs]
    protected def getDefaultRep = Converter
  }

  abstract class ConverterCompanionAbs extends CompanionBase[ConverterCompanionAbs] with ConverterCompanion {
    override def toString = "Converter"
  }
  def Converter: Rep[ConverterCompanionAbs]
  implicit def proxyConverterCompanion(p: Rep[ConverterCompanion]): ConverterCompanion = {
    proxyOps[ConverterCompanion](p)
  }

  // elem for concrete class
  class BaseConverterElem[T, R](val iso: Iso[BaseConverterData[T, R], BaseConverter[T, R]])(implicit eDom: Elem[T], eRange: Elem[R])
    extends ConverterElem[T, R, BaseConverter[T, R]]
    with ViewElem[BaseConverterData[T, R], BaseConverter[T, R]] {
    override def convertConverter(x: Rep[Converter[T, R]]) = BaseConverter(x.convFun)
    override def getDefaultRep = super[ViewElem].getDefaultRep
    override lazy val tag = super[ViewElem].tag
  }

  // state representation type
  type BaseConverterData[T, R] = T => R

  // 3) Iso for concrete class
  class BaseConverterIso[T, R](implicit eDom: Elem[T], eRange: Elem[R])
    extends Iso[BaseConverterData[T, R], BaseConverter[T, R]] {
    override def from(p: Rep[BaseConverter[T, R]]) =
      unmkBaseConverter(p) match {
        case Some((convFun)) => convFun
        case None => !!!
      }
    override def to(p: Rep[T => R]) = {
      val convFun = p
      BaseConverter(convFun)
    }
    lazy val tag = {
      implicit val tagT = eDom.tag
      implicit val tagR = eRange.tag
      weakTypeTag[BaseConverter[T, R]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[BaseConverter[T, R]]](BaseConverter(fun { (x: Rep[T]) => element[R].defaultRepValue }))
    lazy val eTo = new BaseConverterElem[T, R](this)
  }
  // 4) constructor and deconstructor
  abstract class BaseConverterCompanionAbs extends CompanionBase[BaseConverterCompanionAbs] with BaseConverterCompanion {
    override def toString = "BaseConverter"

    def apply[T, R](convFun: Rep[T => R])(implicit eDom: Elem[T], eRange: Elem[R]): Rep[BaseConverter[T, R]] =
      mkBaseConverter(convFun)
    def unapply[T:Elem, R:Elem](p: Rep[BaseConverter[T, R]]) = unmkBaseConverter(p)
  }
  def BaseConverter: Rep[BaseConverterCompanionAbs]
  implicit def proxyBaseConverterCompanion(p: Rep[BaseConverterCompanionAbs]): BaseConverterCompanionAbs = {
    proxyOps[BaseConverterCompanionAbs](p)
  }

  class BaseConverterCompanionElem extends CompanionElem[BaseConverterCompanionAbs] {
    lazy val tag = weakTypeTag[BaseConverterCompanionAbs]
    protected def getDefaultRep = BaseConverter
  }
  implicit lazy val BaseConverterCompanionElem: BaseConverterCompanionElem = new BaseConverterCompanionElem

  implicit def proxyBaseConverter[T, R](p: Rep[BaseConverter[T, R]]): BaseConverter[T, R] =
    proxyOps[BaseConverter[T, R]](p)

  implicit class ExtendedBaseConverter[T, R](p: Rep[BaseConverter[T, R]])(implicit eDom: Elem[T], eRange: Elem[R]) {
    def toData: Rep[BaseConverterData[T, R]] = isoBaseConverter(eDom, eRange).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoBaseConverter[T, R](implicit eDom: Elem[T], eRange: Elem[R]): Iso[BaseConverterData[T, R], BaseConverter[T, R]] =
    new BaseConverterIso[T, R]

  // 6) smart constructor and deconstructor
  def mkBaseConverter[T, R](convFun: Rep[T => R])(implicit eDom: Elem[T], eRange: Elem[R]): Rep[BaseConverter[T, R]]
  def unmkBaseConverter[T:Elem, R:Elem](p: Rep[BaseConverter[T, R]]): Option[(Rep[T => R])]

  // elem for concrete class
  class PairConverterElem[A1, A2, B1, B2](val iso: Iso[PairConverterData[A1, A2, B1, B2], PairConverter[A1, A2, B1, B2]])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends ConverterElem[(A1, A2), (B1, B2), PairConverter[A1, A2, B1, B2]]
    with ViewElem[PairConverterData[A1, A2, B1, B2], PairConverter[A1, A2, B1, B2]] {
    override def convertConverter(x: Rep[Converter[(A1, A2), (B1, B2)]]) = // Converter is not generated by meta
!!!("Cannot convert from Converter to PairConverter: missing fields List(conv1, conv2)")
    override def getDefaultRep = super[ViewElem].getDefaultRep
    override lazy val tag = super[ViewElem].tag
  }

  // state representation type
  type PairConverterData[A1, A2, B1, B2] = (Converter[A1,B1], Converter[A2,B2])

  // 3) Iso for concrete class
  class PairConverterIso[A1, A2, B1, B2](implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends Iso[PairConverterData[A1, A2, B1, B2], PairConverter[A1, A2, B1, B2]] {
    override def from(p: Rep[PairConverter[A1, A2, B1, B2]]) =
      unmkPairConverter(p) match {
        case Some((conv1, conv2)) => Pair(conv1, conv2)
        case None => !!!
      }
    override def to(p: Rep[(Converter[A1,B1], Converter[A2,B2])]) = {
      val Pair(conv1, conv2) = p
      PairConverter(conv1, conv2)
    }
    lazy val tag = {
      implicit val tagA1 = eA1.tag
      implicit val tagA2 = eA2.tag
      implicit val tagB1 = eB1.tag
      implicit val tagB2 = eB2.tag
      weakTypeTag[PairConverter[A1, A2, B1, B2]]
    }
    lazy val defaultRepTo = Default.defaultVal[Rep[PairConverter[A1, A2, B1, B2]]](PairConverter(element[Converter[A1,B1]].defaultRepValue, element[Converter[A2,B2]].defaultRepValue))
    lazy val eTo = new PairConverterElem[A1, A2, B1, B2](this)
  }
  // 4) constructor and deconstructor
  abstract class PairConverterCompanionAbs extends CompanionBase[PairConverterCompanionAbs] with PairConverterCompanion {
    override def toString = "PairConverter"
    def apply[A1, A2, B1, B2](p: Rep[PairConverterData[A1, A2, B1, B2]])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairConverter[A1, A2, B1, B2]] =
      isoPairConverter(eA1, eA2, eB1, eB2).to(p)
    def apply[A1, A2, B1, B2](conv1: Conv[A1,B1], conv2: Conv[A2,B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairConverter[A1, A2, B1, B2]] =
      mkPairConverter(conv1, conv2)
    def unapply[A1:Elem, A2:Elem, B1:Elem, B2:Elem](p: Rep[PairConverter[A1, A2, B1, B2]]) = unmkPairConverter(p)
  }
  def PairConverter: Rep[PairConverterCompanionAbs]
  implicit def proxyPairConverterCompanion(p: Rep[PairConverterCompanionAbs]): PairConverterCompanionAbs = {
    proxyOps[PairConverterCompanionAbs](p)
  }

  class PairConverterCompanionElem extends CompanionElem[PairConverterCompanionAbs] {
    lazy val tag = weakTypeTag[PairConverterCompanionAbs]
    protected def getDefaultRep = PairConverter
  }
  implicit lazy val PairConverterCompanionElem: PairConverterCompanionElem = new PairConverterCompanionElem

  implicit def proxyPairConverter[A1, A2, B1, B2](p: Rep[PairConverter[A1, A2, B1, B2]]): PairConverter[A1, A2, B1, B2] =
    proxyOps[PairConverter[A1, A2, B1, B2]](p)

  implicit class ExtendedPairConverter[A1, A2, B1, B2](p: Rep[PairConverter[A1, A2, B1, B2]])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]) {
    def toData: Rep[PairConverterData[A1, A2, B1, B2]] = isoPairConverter(eA1, eA2, eB1, eB2).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoPairConverter[A1, A2, B1, B2](implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Iso[PairConverterData[A1, A2, B1, B2], PairConverter[A1, A2, B1, B2]] =
    new PairConverterIso[A1, A2, B1, B2]

  // 6) smart constructor and deconstructor
  def mkPairConverter[A1, A2, B1, B2](conv1: Conv[A1,B1], conv2: Conv[A2,B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairConverter[A1, A2, B1, B2]]
  def unmkPairConverter[A1:Elem, A2:Elem, B1:Elem, B2:Elem](p: Rep[PairConverter[A1, A2, B1, B2]]): Option[(Rep[Converter[A1,B1]], Rep[Converter[A2,B2]])]
}

// Seq -----------------------------------
trait ConvertersSeq extends ConvertersDsl  {
  self: ScalanSeq =>
  lazy val Converter: Rep[ConverterCompanionAbs] = new ConverterCompanionAbs with UserTypeSeq[ConverterCompanionAbs, ConverterCompanionAbs] {
    lazy val selfType = element[ConverterCompanionAbs]
  }

  case class SeqBaseConverter[T, R]
      (override val convFun: Rep[T => R])
      (implicit eDom: Elem[T], eRange: Elem[R])
    extends BaseConverter[T, R](convFun)
        with UserTypeSeq[Converter[T,R], BaseConverter[T, R]] {
    lazy val selfType = element[BaseConverter[T, R]].asInstanceOf[Elem[Converter[T,R]]]
  }
  lazy val BaseConverter = new BaseConverterCompanionAbs with UserTypeSeq[BaseConverterCompanionAbs, BaseConverterCompanionAbs] {
    lazy val selfType = element[BaseConverterCompanionAbs]
  }

  def mkBaseConverter[T, R]
      (convFun: Rep[T => R])(implicit eDom: Elem[T], eRange: Elem[R]): Rep[BaseConverter[T, R]] =
      new SeqBaseConverter[T, R](convFun)
  def unmkBaseConverter[T:Elem, R:Elem](p: Rep[BaseConverter[T, R]]) =
    Some((p.convFun))

  case class SeqPairConverter[A1, A2, B1, B2]
      (override val conv1: Conv[A1,B1], override val conv2: Conv[A2,B2])
      (implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends PairConverter[A1, A2, B1, B2](conv1, conv2)
        with UserTypeSeq[Converter[(A1, A2),(B1, B2)], PairConverter[A1, A2, B1, B2]] {
    lazy val selfType = element[PairConverter[A1, A2, B1, B2]].asInstanceOf[Elem[Converter[(A1, A2),(B1, B2)]]]
  }
  lazy val PairConverter = new PairConverterCompanionAbs with UserTypeSeq[PairConverterCompanionAbs, PairConverterCompanionAbs] {
    lazy val selfType = element[PairConverterCompanionAbs]
  }

  def mkPairConverter[A1, A2, B1, B2]
      (conv1: Conv[A1,B1], conv2: Conv[A2,B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairConverter[A1, A2, B1, B2]] =
      new SeqPairConverter[A1, A2, B1, B2](conv1, conv2)
  def unmkPairConverter[A1:Elem, A2:Elem, B1:Elem, B2:Elem](p: Rep[PairConverter[A1, A2, B1, B2]]) =
    Some((p.conv1, p.conv2))
}

// Exp -----------------------------------
trait ConvertersExp extends ConvertersDsl  {
  self: ScalanExp =>
  lazy val Converter: Rep[ConverterCompanionAbs] = new ConverterCompanionAbs with UserTypeDef[ConverterCompanionAbs, ConverterCompanionAbs] {
    lazy val selfType = element[ConverterCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpBaseConverter[T, R]
      (override val convFun: Rep[T => R])
      (implicit eDom: Elem[T], eRange: Elem[R])
    extends BaseConverter[T, R](convFun) with UserTypeDef[Converter[T,R], BaseConverter[T, R]] {
    lazy val selfType = element[BaseConverter[T, R]].asInstanceOf[Elem[Converter[T,R]]]
    override def mirror(t: Transformer) = ExpBaseConverter[T, R](t(convFun))
  }

  lazy val BaseConverter: Rep[BaseConverterCompanionAbs] = new BaseConverterCompanionAbs with UserTypeDef[BaseConverterCompanionAbs, BaseConverterCompanionAbs] {
    lazy val selfType = element[BaseConverterCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object BaseConverterMethods {
    object apply {
      def unapply(d: Def[_]): Option[(Rep[BaseConverter[T, R]], Rep[T]) forSome {type T; type R}] = d match {
        case MethodCall(receiver, method, Seq(x, _*), _) if receiver.elem.isInstanceOf[BaseConverterElem[_, _]] && method.getName == "apply" =>
          Some((receiver, x)).asInstanceOf[Option[(Rep[BaseConverter[T, R]], Rep[T]) forSome {type T; type R}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[BaseConverter[T, R]], Rep[T]) forSome {type T; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `toString`: Method's return type String is not a Rep

    // WARNING: Cannot generate matcher for method `equals`: Method's return type Boolean is not a Rep
  }

  object BaseConverterCompanionMethods {
  }

  def mkBaseConverter[T, R]
    (convFun: Rep[T => R])(implicit eDom: Elem[T], eRange: Elem[R]): Rep[BaseConverter[T, R]] =
    new ExpBaseConverter[T, R](convFun)
  def unmkBaseConverter[T:Elem, R:Elem](p: Rep[BaseConverter[T, R]]) =
    Some((p.convFun))

  case class ExpPairConverter[A1, A2, B1, B2]
      (override val conv1: Conv[A1,B1], override val conv2: Conv[A2,B2])
      (implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends PairConverter[A1, A2, B1, B2](conv1, conv2) with UserTypeDef[Converter[(A1, A2),(B1, B2)], PairConverter[A1, A2, B1, B2]] {
    lazy val selfType = element[PairConverter[A1, A2, B1, B2]].asInstanceOf[Elem[Converter[(A1, A2),(B1, B2)]]]
    override def mirror(t: Transformer) = ExpPairConverter[A1, A2, B1, B2](t(conv1), t(conv2))
  }

  lazy val PairConverter: Rep[PairConverterCompanionAbs] = new PairConverterCompanionAbs with UserTypeDef[PairConverterCompanionAbs, PairConverterCompanionAbs] {
    lazy val selfType = element[PairConverterCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object PairConverterMethods {
    object apply {
      def unapply(d: Def[_]): Option[(Rep[PairConverter[A1, A2, B1, B2]], Rep[(A1, A2)]) forSome {type A1; type A2; type B1; type B2}] = d match {
        case MethodCall(receiver, method, Seq(x, _*), _) if receiver.elem.isInstanceOf[PairConverterElem[_, _, _, _]] && method.getName == "apply" =>
          Some((receiver, x)).asInstanceOf[Option[(Rep[PairConverter[A1, A2, B1, B2]], Rep[(A1, A2)]) forSome {type A1; type A2; type B1; type B2}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairConverter[A1, A2, B1, B2]], Rep[(A1, A2)]) forSome {type A1; type A2; type B1; type B2}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object PairConverterCompanionMethods {
  }

  def mkPairConverter[A1, A2, B1, B2]
    (conv1: Conv[A1,B1], conv2: Conv[A2,B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairConverter[A1, A2, B1, B2]] =
    new ExpPairConverter[A1, A2, B1, B2](conv1, conv2)
  def unmkPairConverter[A1:Elem, A2:Elem, B1:Elem, B2:Elem](p: Rep[PairConverter[A1, A2, B1, B2]]) =
    Some((p.conv1, p.conv2))

  object ConverterMethods {
    object convFun {
      def unapply(d: Def[_]): Option[Rep[Converter[T, R]] forSome {type T; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ConverterElem[_, _, _]] && method.getName == "convFun" =>
          Some(receiver).asInstanceOf[Option[Rep[Converter[T, R]] forSome {type T; type R}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Converter[T, R]] forSome {type T; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[Converter[T, R]], Rep[T]) forSome {type T; type R}] = d match {
        case MethodCall(receiver, method, Seq(x, _*), _) if receiver.elem.isInstanceOf[ConverterElem[_, _, _]] && method.getName == "apply" =>
          Some((receiver, x)).asInstanceOf[Option[(Rep[Converter[T, R]], Rep[T]) forSome {type T; type R}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Converter[T, R]], Rep[T]) forSome {type T; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object ConverterCompanionMethods {
  }
}
