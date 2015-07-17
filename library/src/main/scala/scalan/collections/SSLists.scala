package scalan.collections

import scala.reflect.ClassTag
import scalan._
import scalan.common.Default

trait SSLists extends Base with TypeWrappers { self: ScalanCommunityDsl =>
  type RepSSList[A] = Rep[SSList[A]]

  @ContainerType
  trait SSList[A] extends TypeWrapper[List[A],SSList[A]] { self =>
    implicit def eA: Elem[A]

    def wrappedValueOfBaseType: Rep[List[A]]

//    def map[B: Elem](f: Rep[A => B]): Rep[SSList[B]]
    def map[B:Elem](f: Rep[A => B]): Rep[SSList[B]] =
      methodCallEx[SSList[B]](self,
        this.getClass.getMethod("map", classOf[AnyRef], classOf[Elem[B]]),
        List(f.asInstanceOf[AnyRef], element[B]))
  }

  trait SSListCompanion extends ExCompanion1[SSList] {
    @External def empty[A:Elem]: Rep[SSList[A]]
  }

  def DefaultOfList[A: Elem]: Default[List[A]] = Default.defaultVal(List.empty[A])
  implicit def ctA[A: Elem]: ClassTag[A] = element[A].classTag

//  //redefine implicits from Predef to prevent ambiguity
//  def genericArrayOps[Q](a: scala.Array[Q]): scala.collection.mutable.ArrayOps[Q] = ???
//  def genericWrapArray[T](xs: scala.Array[T]): scala.collection.mutable.WrappedArray[T] = ???
}

trait SSListsDsl extends impl.SSListsAbs { self: ScalanCommunityDsl => }

trait SSListsDslSeq extends impl.SSListsSeq { self: ScalanCommunityDslSeq =>
  trait SeqSSList[A] extends SSListImpl[A] {
    override def map[B: Elem](f: Rep[A => B]): Rep[SSList[B]] = SSListImpl(wrappedValueOfBaseType.map(f))
  }
}

trait SSListsDslExp extends impl.SSListsExp { self1: ScalanCommunityDslExp =>
  trait ExpSSList[A] extends SSListImpl[A] {

  }
}
