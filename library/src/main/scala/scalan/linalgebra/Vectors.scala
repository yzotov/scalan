package scalan.linalgebra

/**
 * Created by Victor Smirnov on 3/12/15.
 */

import scalan._
import scalan.common.OverloadHack.{Overloaded2, Overloaded1}
import scala.annotation.unchecked.uncheckedVariance

trait Vectors { self: ScalanCommunityDsl =>

  type Vector[T] = Rep[AbstractVector[T]]

  trait AbstractVector[T] extends Reifiable[AbstractVector[T]] {

    def length: Rep[Int]
    def items: Rep[Collection[T]]
    def nonZeroIndices: Rep[Collection[Int]]
    def nonZeroValues:  Rep[Collection[T]]
    def nonZeroItems: Rep[Collection[(Int, T)]]
    implicit def eT: Elem[T]
    def zeroValue = eT.defaultRepValue

    def apply(i: Rep[Int]): Rep[T]
    @OverloadId("apply_by_collection")
    def apply(is: Coll[Int])(implicit o: Overloaded1): Vector[T]

    def mapBy[R: Elem](f: Rep[T => R @uncheckedVariance]): Vector[R]
    def filterBy(f: Rep[T @uncheckedVariance => Boolean]): Vector[T]

    def +^(other: Vector[T])(implicit n: Numeric[T]): Vector[T]
    @OverloadId("elementwise_sum_collection")
    def +^(other: Coll[T])(implicit n: Numeric[T], o: Overloaded1): Vector[T] = +^ (DenseVector(other))
    @OverloadId("elementwise_sum_value")
    def +^(other: Rep[T])(implicit n: Numeric[T], o: Overloaded2): Vector[T]

    def -^(other: Vector[T])(implicit n: Numeric[T]): Vector[T]
    @OverloadId("elementwise_diff_collection")
    def -^(other: Coll[T])(implicit n: Numeric[T], o: Overloaded1): Vector[T] = -^ (DenseVector(other))
    @OverloadId("elementwise_diff_value")
    def -^(other: Rep[T])(implicit n: Numeric[T], o: Overloaded2): Vector[T]

    def *^(other: Vector[T])(implicit n: Numeric[T]): Vector[T]
    @OverloadId("elementwise_mult_collection")
    def *^(other: Coll[T])(implicit n: Numeric[T], o: Overloaded1): Vector[T] = *^ (DenseVector(other))
    @OverloadId("elementwise_mult_value")
    def *^(other: Rep[T])(implicit n: Numeric[T], o: Overloaded2): Vector[T]

    def /^(other: Rep[T])(implicit f: Fractional[T]): Vector[T] = *^ (toRep(f.one) / other)

    def *(mat: Rep[AbstractMatrix[T]])(implicit n: Numeric[T], o: Overloaded1): Rep[AbstractMatrix[T]] = ???

    def pow_^(order: Rep[Double])(implicit n: Numeric[T], o: Overloaded2): Vector[T]

    def euclideanNorm(implicit num: Numeric[T]): Rep[Double]

    def reduce(implicit m: RepMonoid[T]): Rep[T]
    def dot(other: Vector[T])(implicit n: Numeric[T]): Rep[T]

    def nonZeroesLength: Rep[Int] = nonZeroItems.length

    def companion: Rep[AbstractVectorCompanion]
  }

  abstract class DenseVector[T](val items: Rep[Collection[T]])
                               (implicit val eT: Elem[T])
    extends AbstractVector[T] {

    def length = items.length
    def nonZeroIndices: Rep[Collection[Int]] = nonZeroItems.map { case Pair(i, v) => i }
    def nonZeroValues:  Rep[Collection[T]] = items.filter(v => v !== zeroValue)
    def nonZeroItems:   Rep[Collection[(Int, T)]] = {
      (Collection.indexRange(length) zip items).filter { case Pair(i, v) => v !== zeroValue }
    }

    def apply(i: Rep[Int]): Rep[T] = items(i)
    @OverloadId("apply_by_collection")
    def apply(is: Coll[Int])(implicit o: Overloaded1): Vector[T] = DenseVector(items(is))

    def mapBy[R: Elem](f: Rep[T => R @uncheckedVariance]): Vector[R] = DenseVector(items.mapBy(f))
    def filterBy(f: Rep[T @uncheckedVariance => Boolean]): Vector[T] = DenseVector(items.map(v => IF (f(v)) THEN v ELSE zeroValue))

    def +^(other: Vector[T])(implicit n: Numeric[T]): Vector[T] = {
      other match {
        case SparseVectorMatcher(nonZeroIndices, nonZeroValues, _) =>
          val nonZeroValuesNew = (nonZeroValues zip items(nonZeroIndices)).map { case Pair(v1, v2) => v1 + v2 }
          DenseVector(items.updateMany(nonZeroIndices, nonZeroValuesNew))
        case _ =>
          DenseVector((items zip other.items).map { case Pair(v1, v2) => v1 + v2 })
      }
    }

    @OverloadId("elementwise_sum_value")
    def +^(other: Rep[T])(implicit n: Numeric[T], o: Overloaded2): Vector[T] = {
      DenseVector(items.map(v => v + other))
    }

    def -^(other: Vector[T])(implicit n: Numeric[T]): Vector[T] = {
      other match {
        case SparseVectorMatcher(nonZeroIndices, nonZeroValues, _) =>
          val nonZeroValuesNew = (nonZeroValues zip items(nonZeroIndices)).map { case Pair(v1, v2) => v1 - v2 }
          DenseVector(items.updateMany(nonZeroIndices, nonZeroValuesNew))
        case _ =>
          DenseVector((items zip other.items).map { case Pair(v1, v2) => v1 - v2 })
      }
    }
    @OverloadId("elementwise_diff_value")
    def -^(other: Rep[T])(implicit n: Numeric[T], o: Overloaded2): Vector[T] = {
      DenseVector(items.map(v => v - other))
    }

    def *^(other: Vector[T])(implicit n: Numeric[T]): Vector[T] = {
      other match {
        case SparseVectorMatcher(nonZeroIndices, nonZeroValues, _) =>
          val nonZeroValuesNew = (nonZeroValues zip items(nonZeroIndices)).map { case Pair(v1, v2) => v1 * v2 }
          DenseVector(items.updateMany(nonZeroIndices, nonZeroValuesNew))
        case _ =>
          DenseVector((items zip other.items).map { case Pair(v1, v2) => v1 * v2 })
      }
    }
    @OverloadId("elementwise_mult_value")
    def *^(other: Rep[T])(implicit n: Numeric[T], o: Overloaded2): Vector[T] = {
      DenseVector(items.map(v => v * other))
    }

    def pow_^(order: Rep[Double])(implicit n: Numeric[T], o: Overloaded2): Vector[T] = {
      DenseVector(items.map(v => Math.pow(v.toDouble, order).asRep[T]))
    }

    def reduce(implicit m: RepMonoid[T]): Rep[T] = items.reduce(m)
    def dot(other: Vector[T])(implicit n: Numeric[T]): Rep[T] = {
      other match {
        case SparseVectorMatcher(nonZeroIndices, nonZeroValues, _) =>
          (items(nonZeroIndices) zip nonZeroValues).map { case Pair(v1, v2) => v1 * v2 }.reduce
        case _ =>
          (other.items zip items).map { case Pair(v1, v2) => v1 * v2 }.reduce
      }
    }

    def euclideanNorm(implicit num: Numeric[T]): Rep[Double] = Math.sqrt(items.map(v => v * v).reduce.asRep[Double])

    def companion = DenseVector
  }

  abstract class SparseVector[T](val nonZeroIndices: Rep[Collection[Int]],
                                 val nonZeroValues: Rep[Collection[T]],
                                 val length: Rep[Int])(implicit val eT: Elem[T])
    extends AbstractVector[T] {

    def items: Rep[Collection[T]] = Collection.replicate(length, zeroValue).updateMany(nonZeroIndices, nonZeroValues)
    def nonZeroItems: Rep[Collection[(Int, T)]] = nonZeroIndices zip nonZeroValues

    def apply(i: IntRep): Rep[T] = {
      val zero = toRep(0)
      IF (nonZeroIndices.length > zero) THEN {
        val k = binarySearch(i, nonZeroIndices)
        IF (k >= zero) THEN nonZeroValues(k) ELSE zeroValue
      } ELSE zeroValue
    }

    @OverloadId("apply_by_collection")
    def apply(is: Coll[Int])(implicit o: Overloaded1): Vector[T] = ??? // TODO: need efficient way to get value by index

    def mapBy[R: Elem](f: Rep[T => R @uncheckedVariance]): Vector[R] = {
      IF (f(zeroValue) === element[R].defaultRepValue) THEN {
        SparseVector(nonZeroIndices, nonZeroValues.mapBy(f), length)
      } ELSE {
        ???
      }
    }
    def filterBy(f: Rep[T @uncheckedVariance => Boolean]): Vector[T] = {
      val filteredItems = nonZeroItems.filter { v => f(v._2) }
      SparseVector(filteredItems.as, filteredItems.bs, length)
    }

    def +^(other: Vector[T])(implicit n: Numeric[T]): Vector[T] = {
      other match {
        case SparseVectorMatcher(nonZeroIndices1, nonZeroValues1, _) =>
          val newItems = (nonZeroIndices zip nonZeroValues) outerSum (nonZeroIndices1 zip nonZeroValues1)
          SparseVector(newItems.as, newItems.bs, length)
        case SparseVector1Matcher(nonZeroItems1, _) =>
          val newItems = (nonZeroIndices zip nonZeroValues) outerSum (nonZeroItems1.as zip nonZeroItems1.bs)
          SparseVector(newItems.as, newItems.bs, length)
        case _ =>
          other +^ self
      }
    }

    @OverloadId("elementwise_sum_value")
    def +^(other: Rep[T])(implicit n: Numeric[T], o: Overloaded2): Vector[T] = {
      DenseVector(items.map(v => v + other))
    }

    def -^(other: Vector[T])(implicit n: Numeric[T]): Vector[T] = {
      // TODO: I don't like constructing items in this method
      other match {
        case SparseVectorMatcher(nonZeroIndices1, nonZeroValues1, _) =>
          val newItems = (nonZeroIndices zip nonZeroValues) outerSubtr (nonZeroIndices1 zip nonZeroValues1)
          SparseVector(newItems.as, newItems.bs, length)
        case DenseVectorMatcher(items) =>
          val nonZeroValuesNew = (nonZeroValues zip items(nonZeroIndices)).map { case Pair(v1, v2) => v1 - v2 }
          DenseVector(items.updateMany(nonZeroIndices, nonZeroValuesNew))
        case _ =>
          (other -^ self) *^ n.negate(n.one)
      }
    }
    @OverloadId("elementwise_diff_value")
    def -^(other: Rep[T])(implicit n: Numeric[T], o: Overloaded2): Vector[T] = {
      DenseVector(items.map(v => v - other))
    }

    def *^(other: Vector[T])(implicit n: Numeric[T]): Vector[T] = {
      other match {
        case SparseVectorMatcher(nonZeroIndices1, nonZeroValues1, _) =>
          val newItems = (nonZeroIndices zip nonZeroValues) innerMult (nonZeroIndices1 zip nonZeroValues1)
          SparseVector(newItems.as, newItems.bs, length)
        case _ =>
          other *^ self
      }
    }
    @OverloadId("elementwise_mult_value")
    def *^(other: Rep[T])(implicit n: Numeric[T], o: Overloaded2): Vector[T] = {
      SparseVector(nonZeroIndices, nonZeroValues.map(v => v * other), length)
    }

    def pow_^(order: Rep[Double])(implicit n: Numeric[T], o: Overloaded2): Vector[T] = {
      SparseVector(nonZeroIndices, nonZeroValues.map(v => Math.pow(v.toDouble, order).asRep[T]), length)
    }

    def reduce(implicit m: RepMonoid[T]): Rep[T] = {
      if (m.zero == zeroValue) nonZeroValues.reduce(m) else items.reduce(m)
    }  //TODO: it's inefficient

    def dot(other: Rep[AbstractVector[T]])(implicit n: Numeric[T]): Rep[T] = {
      other match {
        case SparseVectorMatcher(nonZeroIndices1, nonZeroValues1, _) =>
          // TODO implement innerJoin and uncomment
          //sv => innerJoin(nonZeroIndices, nonZeroValues, sv.nonZeroIndices, sv.nonZeroValues).bs.reduce
          dotSparse(nonZeroIndices, nonZeroValues, nonZeroIndices1, nonZeroValues1)
        case _ =>
          other.dot(self)
      }
    }

    def euclideanNorm(implicit num: Numeric[T]): Rep[Double] = Math.sqrt(nonZeroValues.map(v => v * v).reduce.asRep[Double])

    def companion = SparseVector
  }

  abstract class SparseVector1[T](val nonZeroItems: Rep[Collection[(Int, T)]],
                                  val length: Rep[Int])(implicit val eT: Elem[T])
    extends AbstractVector[T] {

    def items: Rep[Collection[T]] = Collection.replicate(length, zeroValue).updateMany(nonZeroIndices, nonZeroValues)
    def nonZeroIndices: Rep[Collection[Int]] = nonZeroItems.as
    def nonZeroValues: Rep[Collection[T]] = nonZeroItems.bs

    def apply(i: IntRep): Rep[T] = {
      val zero = toRep(0)
      IF (nonZeroIndices.length > zero) THEN {
        val k = binarySearch(i, nonZeroIndices)
        IF (k >= zero) THEN nonZeroValues(k) ELSE zeroValue
      } ELSE zeroValue
    }

    @OverloadId("apply_by_collection")
    def apply(is: Coll[Int])(implicit o: Overloaded1): Vector[T] = ??? // TODO: need efficient way to get value by index

    def mapBy[R: Elem](f: Rep[T => R @uncheckedVariance]): Vector[R] = {
      IF (f(zeroValue) === element[R].defaultRepValue) THEN {
        SparseVector(nonZeroIndices, nonZeroValues.mapBy(f), length)
      } ELSE {
        ???
      }
    }
    def filterBy(f: Rep[T @uncheckedVariance => Boolean]): Vector[T] = {
      val filteredItems = nonZeroItems.filter { v => f(v._2) }
      SparseVector1(filteredItems, length)
    }

    def +^(other: Vector[T])(implicit n: Numeric[T]): Vector[T] = {
      other match {
        case SparseVector1Matcher(nonZeroItems1, _) =>
          val newItems = (nonZeroIndices zip nonZeroValues) outerSum (nonZeroItems1.as zip nonZeroItems1.bs)
          SparseVector1(newItems, length)
        case _ =>
          other +^ self
      }
    }

    @OverloadId("elementwise_sum_value")
    def +^(other: Rep[T])(implicit n: Numeric[T], o: Overloaded2): Vector[T] = {
      DenseVector(items.map(v => v + other))
    }

    def -^(other: Vector[T])(implicit n: Numeric[T]): Vector[T] = {
      // TODO: I don't like constructing items in this method
      other match {
        case SparseVector1Matcher(nonZeroItems1, _) =>
          val newItems = (nonZeroIndices zip nonZeroValues) outerSubtr (nonZeroItems1.as zip nonZeroItems1.bs)
          SparseVector1(newItems, length)
        case DenseVectorMatcher(items) =>
          val nonZeroValuesNew = (nonZeroValues zip items(nonZeroIndices)).map { case Pair(v1, v2) => v1 - v2 }
          DenseVector(items.updateMany(nonZeroIndices, nonZeroValuesNew))
        case _ =>
          (other -^ self) *^ n.negate(n.one)
      }
    }
    @OverloadId("elementwise_diff_value")
    def -^(other: Rep[T])(implicit n: Numeric[T], o: Overloaded2): Vector[T] = {
      DenseVector(items.map(v => v - other))
    }

    def *^(other: Vector[T])(implicit n: Numeric[T]): Vector[T] = {
      other match {
        case SparseVector1Matcher(nonZeroItems1, _) =>
          val newItems = (nonZeroIndices zip nonZeroValues) innerMult (nonZeroItems1.as zip nonZeroItems1.bs)
          SparseVector1(newItems, length)
        case _ =>
          other *^ self
      }
    }
    @OverloadId("elementwise_mult_value")
    def *^(other: Rep[T])(implicit n: Numeric[T], o: Overloaded2): Vector[T] = {
      SparseVector1(nonZeroIndices zip nonZeroValues.map(v => v * other), length)
    }

    def pow_^(order: Rep[Double])(implicit n: Numeric[T], o: Overloaded2): Vector[T] = {
      SparseVector1(nonZeroIndices zip nonZeroValues.map(v => Math.pow(v.toDouble, order).asRep[T]), length)
    }

    def reduce(implicit m: RepMonoid[T]): Rep[T] = items.reduce(m)  //TODO: it's inefficient

    def dot(other: Rep[AbstractVector[T]])(implicit n: Numeric[T]): Rep[T] = {
      other match {
        case SparseVectorMatcher(nonZeroIndices1, nonZeroValues1, _) =>
          // TODO implement innerJoin and uncomment
          //sv => innerJoin(nonZeroIndices, nonZeroValues, sv.nonZeroIndices, sv.nonZeroValues).bs.reduce
          dotSparse(nonZeroIndices, nonZeroValues, nonZeroIndices1, nonZeroValues1)
        case _ =>
          other.dot(self)
      }
    }

    def euclideanNorm(implicit num: Numeric[T]): Rep[Double] = Math.sqrt(nonZeroValues.map(v => v * v).reduce.asRep[Double])

    def companion = SparseVector
  }

  trait AbstractVectorCompanion extends TypeFamily1[AbstractVector] {
    def zero[T: Elem](len: Rep[Int]): Vector[T] = ??? //DenseVector.zero[T](len)
    def fromSparseData[T: Elem](nonZeroIndices: Rep[Collection[Int]],
                                nonZeroValues: Rep[Collection[T]], length: Rep[Int]): Vector[T] = ???
  }

  trait DenseVectorCompanion extends ConcreteClass1[AbstractVector] with AbstractVectorCompanion {
    override def zero[T: Elem](len: Rep[Int]): Vector[T] = {
      val zeroV = element[T].defaultRepValue
      DenseVector(Collection.replicate(len, zeroV))
    }
    override def fromSparseData[T: Elem](nonZeroIndices: Rep[Collection[Int]],
                                nonZeroValues: Rep[Collection[T]], length: Rep[Int]): Vector[T] = ???
  }

  trait SparseVectorCompanion extends ConcreteClass1[AbstractVector] with AbstractVectorCompanion {
    def apply[T: Elem](items: Rep[Collection[T]])(implicit n: Numeric[T], o: Overloaded1): Rep[SparseVector[T]] = {
      val nonZeroItems =
        (Collection.indexRange(items.length) zip items).filter { case Pair(i, v) => v !== n.zero }
      SparseVector(nonZeroItems, items.length)
    }
    @OverloadId("SparseVectorCompanion_apply_nonZeroItems")
    def apply[T: Elem](nonZeroItems: Rep[Collection[(Int, T)]], length: Rep[Int])
                      (implicit n: Numeric[T], o: Overloaded2): Rep[SparseVector[T]] = {
      SparseVector(nonZeroItems.as, nonZeroItems.bs, length)
    }
    override def zero[T: Elem](len: Rep[Int]) = SparseVector(emptyColl[Int], emptyColl[T], len)
    override def fromSparseData[T: Elem](nonZeroIndices: Rep[Collection[Int]], nonZeroValues: Rep[Collection[T]],
                                         length: Rep[Int]): Vector[T] = SparseVector(nonZeroIndices, nonZeroValues, length)
  }

  trait SparseVector1Companion extends ConcreteClass1[AbstractVector] with AbstractVectorCompanion {
    def apply[T: Elem](items: Rep[Collection[T]])(implicit n: Numeric[T], o: Overloaded1): Rep[SparseVector1[T]] = {
      val nonZeroItems =
        (Collection.indexRange(items.length) zip items).filter { case Pair(i, v) => v !== n.zero }
      SparseVector1(nonZeroItems, items.length)
    }
    @OverloadId("SparseVector1Companion_apply_nonZeroItems")
    def apply[T: Elem](nonZeroIndices: Rep[Collection[Int]], nonZeroValues: Rep[Collection[T]], length: Rep[Int])
                      (implicit n: Numeric[T], o: Overloaded2): Rep[SparseVector1[T]] = {
      SparseVector1(nonZeroIndices zip nonZeroValues, length)
    }
    override def zero[T: Elem](len: Rep[Int]) = SparseVector(emptyColl[Int], emptyColl[T], len)
    override def fromSparseData[T: Elem](nonZeroIndices: Rep[Collection[Int]], nonZeroValues: Rep[Collection[T]],
                                         length: Rep[Int]): Vector[T] = SparseVector1(nonZeroIndices zip nonZeroValues, length)
  }
}

trait VectorsDsl extends impl.VectorsAbs { self: ScalanCommunityDsl =>

  type VectorCompanion = Rep[AbstractVectorCompanion]

  def dotSparse[T: Elem](xIndices: Coll[Int], xValues: Coll[T], yIndices: Coll[Int], yValues: Coll[T])
                        (implicit n: Numeric[T]): Rep[T]

  def dotMerge[T: Elem](xItems: Coll[T], yIndices: Coll[Int], yValues: Coll[T])
                       (implicit n: Numeric[T]): Rep[T] = {
    (xItems(yIndices) zip yValues).map { case Pair(x, y) => x * y }.reduce
  }

  /*def innerJoin[T: Elem](xIndices: Coll[Int], xValues: Coll[T], yIndices: Coll[Int], yValues: Coll[T],
                         f: Rep[((T, T)) => T])(implicit n: Numeric[T]): PairColl[Int, T] =
    (xIndices zip xValues).innerJoin(yIndices zip yValues, f)

  def outerJoin[T: Elem](xIndices: Coll[Int], xValues: Coll[T], yIndices: Coll[Int], yValues: Coll[T],
                         f: Rep[((T, T)) => T])(implicit n: Numeric[T]): PairColl[Int, T] =
     (xIndices zip xValues).outerJoin(yIndices zip yValues, f)*/

  def binarySearch(index: IntRep, indices: Coll[Int]): IntRep

  implicit class VectorExtensions[T](vector: Vector[T]) {
    implicit def eItem: Elem[T] = vector.selfType1.asInstanceOf[AbstractVectorElem[T, _]].eT

    def map[R: Elem](f: Rep[T] => Rep[R]): Vector[R] = vector.mapBy(fun(f))

    def filter(f: Rep[T] => Rep[Boolean]): Vector[T] = vector.filterBy(fun(f))

    //def flatMap[R: Elem](f: Rep[T] => Coll[R]): Matrix[R] = matrix.flatMapBy(fun(f))
  }
}

trait VectorsDslSeq extends impl.VectorsSeq { self: ScalanCommunityDslSeq =>

  def dotSparse[T: Elem](xIndices: Coll[Int], xValues: Coll[T], yIndices: Coll[Int], yValues: Coll[T])
                        (implicit n: Numeric[T]): Rep[T] = {
    var result = n.zero
    val yMap = yIndices.arr.zip(yValues.arr).toMap
    xIndices.arr.zip(xValues.arr).foldLeft(n.zero) {
      case (acc, (i, x)) =>
      yMap.get(i) match {
        case Some(y) => acc + x * y
        case None => acc
      }
    }
  }

  def binarySearch(index: IntRep, indices: Coll[Int]): IntRep = {
    val zero = 0
    val one = 1
    val two = 2
    def check(start: Int, end: Int): Int = {
      if (end - start < two) {
        if (index === indices(start)) start else {
          if (index === indices(end)) end else -1
        }
      } else {
        val middle = (start + end) div two
        if (index === indices(middle)) middle else {
          if (index < indices(middle)) check(start, middle - one) else check(middle + one, end)
        }
      }
    }
    check(zero, Math.min(index, indices.length - one))
  }
}

trait VectorsDslExp extends impl.VectorsExp { self: ScalanCommunityDslExp =>
  def dotSparse[T: Elem](xIndices: Coll[Int], xValues: Coll[T], yIndices: Coll[Int], yValues: Coll[T])
                        (implicit n: Numeric[T]): Rep[T] = {
    DotSparse(xIndices.arr, xValues.arr, yIndices.arr, yValues.arr)
  }

  case class DotSparse[T](xIndices: Arr[Int], xValues: Arr[T], yIndices: Arr[Int], yValues: Arr[T])
                         (implicit val n: Numeric[T], selfType: Elem[T]) extends BaseDef[T] {
    override def mirror(f: Transformer) = DotSparse(f(xIndices), f(xValues), f(yIndices), f(yValues))
  }

  def binarySearch(index: IntRep, indices: Coll[Int]): IntRep = array_binary_search(index, indices.arr)
}
