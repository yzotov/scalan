package scalan.collections

import scala.reflect.ClassTag
import scalan._
import scalan.common.Default

trait SSLists extends Base with TypeWrappers with scalan.Scalan { self: ScalanCommunityDsl =>
  type RepSSList[A] = Rep[SSList[A]]

  @ContainerType @FunctorType
  trait SSList[A] extends TypeWrapper[List[A],SSList[A]] { self =>
    implicit def eA: Elem[A]

    def wrappedValueOfBaseType: Rep[List[A]]

    @External def head: Rep[A]
    @External def tail: Rep[SSList[A]]
    @External def apply(i: Rep[Int]): Rep[A]
    @External def length: Rep[Int]
    @External def toArray: Rep[Array[A]]
    @External(methodName = "map") def map[B: Elem](f: Rep[A => B]): Rep[SSList[B]]
  }

  trait SSListCompanion extends ExCompanion1[SSList] {
    @External def empty[A:Elem]: Rep[SSList[A]]
    @External def apply[A: Elem](@ArgList xs: Rep[Array[A]]): Rep[SSList[A]]
  }

  def DefaultOfList[A: Elem]: Default[List[A]] = Default.defaultVal(List.empty[A])
  implicit def ctA[A: Elem]: ClassTag[A] = element[A].classTag

  //redefine implicits to prevent ambiguity
  override val listFunctor = new Functor[List] with ListContainer {
    def map[A:Elem,B:Elem](xs: Rep[List[A]])(f: Rep[A] => Rep[B]) = ???
  }
}

trait SSListsDsl extends impl.SSListsAbs { self: ScalanCommunityDsl => }

trait SSListsDslSeq extends impl.SSListsSeq { self: ScalanCommunityDslSeq =>
  trait SeqSSList[A] extends SSListImpl[A] {
    override def map[B: Elem](f: Rep[A => B]): Rep[SSList[B]] = SSListImpl(wrappedValueOfBaseType.map(f))
  }
}

trait SSListsDslExp extends impl.SSListsExp { self1: ScalanCommunityDslExp => }
