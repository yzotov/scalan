package scalan.monads

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait CoproductsAbs extends Coproducts with scalan.Scalan {
  self: MonadsDsl =>

  // single proxy for each type family
  implicit def proxyCoproduct[F[_], G[_], A](p: Rep[Coproduct[F, G, A]]): Coproduct[F, G, A] = {
    proxyOps[Coproduct[F, G, A]](p)(scala.reflect.classTag[Coproduct[F, G, A]])
  }

  // familyElem
  class CoproductElem[F[_], G[_], A, To <: Coproduct[F, G, A]](implicit val cF: Cont[F], val cG: Cont[G], val eA: Elem[A])
    extends EntityElem[To] {
    lazy val parent: Option[Elem[_]] = None
    lazy val entityDef: STraitOrClassDef = {
      val module = getModules("Coproducts")
      module.entities.find(_.name == "Coproduct").get
    }
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("F" -> Right(cF.asInstanceOf[SomeCont]), "G" -> Right(cG.asInstanceOf[SomeCont]), "A" -> Left(eA))
    }
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[Coproduct[F, G, A]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Reifiable[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[Coproduct[F, G, A]] => convertCoproduct(x) }
      tryConvert(element[Coproduct[F, G, A]], this, x, conv)
    }

    def convertCoproduct(x : Rep[Coproduct[F, G, A]]): Rep[To] = {
      assert(x.selfType1.asInstanceOf[Element[_]] match { case _: CoproductElem[_, _, _, _] => true; case _ => false })
      x.asRep[To]
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def coproductElement[F[_], G[_], A](implicit cF: Cont[F], cG: Cont[G], eA: Elem[A]): Elem[Coproduct[F, G, A]] =
    new CoproductElem[F, G, A, Coproduct[F, G, A]]

  implicit case object CoproductCompanionElem extends CompanionElem[CoproductCompanionAbs] {
    lazy val tag = weakTypeTag[CoproductCompanionAbs]
    protected def getDefaultRep = Coproduct
  }

  abstract class CoproductCompanionAbs extends CompanionBase[CoproductCompanionAbs] with CoproductCompanion {
    override def toString = "Coproduct"
  }
  def Coproduct: Rep[CoproductCompanionAbs]
  implicit def proxyCoproductCompanion(p: Rep[CoproductCompanion]): CoproductCompanion =
    proxyOps[CoproductCompanion](p)

  // elem for concrete class
  class CoproductImplElem[F[_], G[_], A](val iso: Iso[CoproductImplData[F, G, A], CoproductImpl[F, G, A]])(implicit cF: Cont[F], cG: Cont[G], eA: Elem[A])
    extends CoproductElem[F, G, A, CoproductImpl[F, G, A]]
    with ConcreteElem[CoproductImplData[F, G, A], CoproductImpl[F, G, A]] {
    override lazy val parent: Option[Elem[_]] = Some(coproductElement(container[F], container[G], element[A]))
    override lazy val entityDef = {
      val module = getModules("Coproducts")
      module.concreteSClasses.find(_.name == "CoproductImpl").get
    }
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("F" -> Right(cF.asInstanceOf[SomeCont]), "G" -> Right(cG.asInstanceOf[SomeCont]), "A" -> Left(eA))
    }

    override def convertCoproduct(x: Rep[Coproduct[F, G, A]]) = CoproductImpl(x.run)
    override def getDefaultRep = super[ConcreteElem].getDefaultRep
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[CoproductImpl[F, G, A]]
    }
  }

  // state representation type
  type CoproductImplData[F[_], G[_], A] = Either[F[A],G[A]]

  // 3) Iso for concrete class
  class CoproductImplIso[F[_], G[_], A](implicit cF: Cont[F], cG: Cont[G], eA: Elem[A])
    extends Iso[CoproductImplData[F, G, A], CoproductImpl[F, G, A]] {
    override def from(p: Rep[CoproductImpl[F, G, A]]) =
      p.run
    override def to(p: Rep[Either[F[A],G[A]]]) = {
      val run = p
      CoproductImpl(run)
    }
    lazy val defaultRepTo: Rep[CoproductImpl[F, G, A]] = CoproductImpl(element[Either[F[A],G[A]]].defaultRepValue)
    lazy val eTo = new CoproductImplElem[F, G, A](this)
  }
  // 4) constructor and deconstructor
  abstract class CoproductImplCompanionAbs extends CompanionBase[CoproductImplCompanionAbs] with CoproductImplCompanion {
    override def toString = "CoproductImpl"

    def apply[F[_], G[_], A](run: Rep[Either[F[A],G[A]]])(implicit cF: Cont[F], cG: Cont[G], eA: Elem[A]): Rep[CoproductImpl[F, G, A]] =
      mkCoproductImpl(run)
  }
  object CoproductImplMatcher {
    def unapply[F[_], G[_], A](p: Rep[Coproduct[F, G, A]]) = unmkCoproductImpl(p)
  }
  def CoproductImpl: Rep[CoproductImplCompanionAbs]
  implicit def proxyCoproductImplCompanion(p: Rep[CoproductImplCompanionAbs]): CoproductImplCompanionAbs = {
    proxyOps[CoproductImplCompanionAbs](p)
  }

  implicit case object CoproductImplCompanionElem extends CompanionElem[CoproductImplCompanionAbs] {
    lazy val tag = weakTypeTag[CoproductImplCompanionAbs]
    protected def getDefaultRep = CoproductImpl
  }

  implicit def proxyCoproductImpl[F[_], G[_], A](p: Rep[CoproductImpl[F, G, A]]): CoproductImpl[F, G, A] =
    proxyOps[CoproductImpl[F, G, A]](p)

  implicit class ExtendedCoproductImpl[F[_], G[_], A](p: Rep[CoproductImpl[F, G, A]])(implicit cF: Cont[F], cG: Cont[G], eA: Elem[A]) {
    def toData: Rep[CoproductImplData[F, G, A]] = isoCoproductImpl(cF, cG, eA).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoCoproductImpl[F[_], G[_], A](implicit cF: Cont[F], cG: Cont[G], eA: Elem[A]): Iso[CoproductImplData[F, G, A], CoproductImpl[F, G, A]] =
    new CoproductImplIso[F, G, A]

  // 6) smart constructor and deconstructor
  def mkCoproductImpl[F[_], G[_], A](run: Rep[Either[F[A],G[A]]])(implicit cF: Cont[F], cG: Cont[G], eA: Elem[A]): Rep[CoproductImpl[F, G, A]]
  def unmkCoproductImpl[F[_], G[_], A](p: Rep[Coproduct[F, G, A]]): Option[(Rep[Either[F[A],G[A]]])]

  registerModule(scalan.meta.ScalanCodegen.loadModule(Coproducts_Module.dump))
}

// Seq -----------------------------------
trait CoproductsSeq extends CoproductsDsl with scalan.ScalanSeq {
  self: MonadsDslSeq =>
  lazy val Coproduct: Rep[CoproductCompanionAbs] = new CoproductCompanionAbs with UserTypeSeq[CoproductCompanionAbs] {
    lazy val selfType = element[CoproductCompanionAbs]
  }

  case class SeqCoproductImpl[F[_], G[_], A]
      (override val run: Rep[Either[F[A],G[A]]])
      (implicit cF: Cont[F], cG: Cont[G], eA: Elem[A])
    extends CoproductImpl[F, G, A](run)
        with UserTypeSeq[CoproductImpl[F, G, A]] {
    lazy val selfType = element[CoproductImpl[F, G, A]]
  }
  lazy val CoproductImpl = new CoproductImplCompanionAbs with UserTypeSeq[CoproductImplCompanionAbs] {
    lazy val selfType = element[CoproductImplCompanionAbs]
  }

  def mkCoproductImpl[F[_], G[_], A]
      (run: Rep[Either[F[A],G[A]]])(implicit cF: Cont[F], cG: Cont[G], eA: Elem[A]): Rep[CoproductImpl[F, G, A]] =
      new SeqCoproductImpl[F, G, A](run)
  def unmkCoproductImpl[F[_], G[_], A](p: Rep[Coproduct[F, G, A]]) = p match {
    case p: CoproductImpl[F, G, A] @unchecked =>
      Some((p.run))
    case _ => None
  }
}

// Exp -----------------------------------
trait CoproductsExp extends CoproductsDsl with scalan.ScalanExp {
  self: MonadsDslExp =>
  lazy val Coproduct: Rep[CoproductCompanionAbs] = new CoproductCompanionAbs with UserTypeDef[CoproductCompanionAbs] {
    lazy val selfType = element[CoproductCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpCoproductImpl[F[_], G[_], A]
      (override val run: Rep[Either[F[A],G[A]]])
      (implicit cF: Cont[F], cG: Cont[G], eA: Elem[A])
    extends CoproductImpl[F, G, A](run) with UserTypeDef[CoproductImpl[F, G, A]] {
    lazy val selfType = element[CoproductImpl[F, G, A]]
    override def mirror(t: Transformer) = ExpCoproductImpl[F, G, A](t(run))
  }

  lazy val CoproductImpl: Rep[CoproductImplCompanionAbs] = new CoproductImplCompanionAbs with UserTypeDef[CoproductImplCompanionAbs] {
    lazy val selfType = element[CoproductImplCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object CoproductImplMethods {
  }

  object CoproductImplCompanionMethods {
  }

  def mkCoproductImpl[F[_], G[_], A]
    (run: Rep[Either[F[A],G[A]]])(implicit cF: Cont[F], cG: Cont[G], eA: Elem[A]): Rep[CoproductImpl[F, G, A]] =
    new ExpCoproductImpl[F, G, A](run)
  def unmkCoproductImpl[F[_], G[_], A](p: Rep[Coproduct[F, G, A]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CoproductImplElem[F, G, A] @unchecked =>
      Some((p.asRep[CoproductImpl[F, G, A]].run))
    case _ =>
      None
  }

  object CoproductMethods {
    object run {
      def unapply(d: Def[_]): Option[Rep[Coproduct[F, G, A]] forSome {type F[_]; type G[_]; type A}] = d match {
        case MethodCall(receiver, method, _, _) if (receiver.elem.asInstanceOf[Element[_]] match { case _: CoproductElem[_, _, _, _] => true; case _ => false }) && method.getName == "run" =>
          Some(receiver).asInstanceOf[Option[Rep[Coproduct[F, G, A]] forSome {type F[_]; type G[_]; type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Coproduct[F, G, A]] forSome {type F[_]; type G[_]; type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object CoproductCompanionMethods {
  }
}

object Coproducts_Module {
  val packageName = "scalan.monads"
  val name = "Coproducts"
  val dump = "H4sIAAAAAAAAALVWPYwbRRR+Xp/Pt/aRhAiddBKIu5MBgcA+gUSKKyLH8Vkg34+8kYhMRDRej30Tdmf2ZsanNUUKSugQLULp09FQ0SEhCioESNRUIRQRkArEzHj/bOy7QyIuRjuzb7/35vveN+P7D6EgOLwoXOQhWvWxRFXHPNeFrDhNKokc77H+yMPX8eDDtS/dPXpNWHCxC8tHSFwXXhfsyUMzDJJnBx+3wUbUxUIyLiRstk2Gmss8D7uSMFojvj+SqOfhWpsIudOGpR7rj4/hLuTacMll1OVYYqfhISGwiNZXsK6IJHPbzMcHQZqD1vQuapld3OCISFW+ynFpEt/BgTOmjI59CRei0g4CXZaKKRI/YFzGKYoK7oj14+kSRWoBLrfvoBNUUymGNUdyQofqy3KA3PfREO+rEB2+pAoW2BvcGAdmnm9DSeBjRdBbfuCZlTAAAKXA66aIaspPNeGnqvmpOJgT5JEPkH55yFk4hskvlwcIAwXx6hkQMQJu0n7lo1vuu4+dsm/pj0NdStHscFkBPb+gG4wUisdvOp+IR617VywodaFERL0nJEeuzEoesVVGlDJpak4IRHyo1NpapJbJUlcxMy1hu8wPEFVIEZWrSiePuETqYL22GqmzgPqiDHAcmguDXLLfjQX7NX3TQJ53+GD9tRd+bd60wJpOYStIRzU+j0El2A0WcGUYV0b4erwoIbebkqynrelp3Uz1YIfpWDyluoSnlx781v96G25ZCbtRMecTVEEUxE8/lL9/+aoFK13T/rseGnYVwaLpYf+ANxiVXVhhJ5hP3hRPkKef5gpc7OMBGnkyoj3LV17xJWFjoVEDrMncMabIxQSUJ329zyiu7B5W/nS+/fS+blsOq5M3E+f+Ta789fOFgTQdLSHPRzRmN6/8Pq3GcpPII8xnJZqZZ0VJhTslaP6oN1GalOowHz+99Yi8d+9jafTKhdNnyUHvjjLvjvlu4xTp4jPtj+629fv6j59bYCuFekT6KKhsn9OJT9BdkLCSDptKlrXEHvoIbGSTbqaOWM9w/Gwu7gUTJMFyd2Pyl3RnzvVZRrQ5AK3TAFpnA+B6AqAdcmZHSHhqat8GJ7Hnc4s0NoyuddrPeA+vfmVB4W0oDJTrRBsKPTai/VgqdVlKHMpr8VpuWiolDeLIT6Qxvw1IOZ9p/c7ciNuztMwPa50OpId3zof0bxoz0G/CNOl2B5MB0XfdzPr/eRxHdk5D34hqWdDsl5Pkcxp96q7INsRiRs4Q6T9w+yRV0uPtaSwVWEqFUG6IO55R1BcRlRy2FhjBiY4add7dffzZ/ivfffGLuYpL+tBS1wJN/qylbR/OnPj2nsml/ntlClb+1ceYKfYfX2yV/AsLAAA="
}
}

