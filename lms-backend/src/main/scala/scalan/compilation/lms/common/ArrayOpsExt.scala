package scalan.compilation.lms.common

import java.util.HashMap

import scala.reflect.SourceContext
import scala.virtualization.lms.common.{ArrayOpsExp, ScalaGenBase}
import scala.virtualization.lms.internal.Transforming
import scalan.compilation.lms.LmsBackendFacade
import scalan.compilation.lms.cxx.sharedptr.CxxShptrCodegen

trait ArrayOpsExtExp extends Transforming { self: LmsBackendFacade =>

  def array_new[A: Manifest](len: Rep[Int]): Rep[Array[A]] = ArrayNew[A](len)

  def mapFromArray[K: Manifest, V: Manifest](arr: Exp[Array[(K, V)]]): Exp[HashMap[K, V]] = {
    val h = HashMap[K, V]()
    for (pair <- arr) {
      h.update(pair._1, pair._2)
    }
    h
  }

  def arrayGet[A: Manifest](a: Exp[Array[A]], i: Exp[Int]): Exp[A] = {
    a.at(i)
  }

  def arrayGather[A: Manifest](a: Exp[Array[A]], idxs: Exp[Array[Int]]): Exp[Array[A]] = {
    array(idxs.length)(i => a.at(idxs.at(i)))
  }

  def arrayLength[A: Manifest](a: Exp[Array[A]]): Exp[Int] = {
    a.length
  }

  def mapArray[A: Manifest, B: Manifest](a: Exp[Array[A]], f: Rep[A] => Rep[B]): Exp[Array[B]] = {
    //    a.map(f)
    array(a.length)(i => f(a.at(i)))
  }

//  def flatMapArray[A: Manifest, B: Manifest](arr: Exp[Array[A]], f: Rep[A] => Rep[Array[B]]): Exp[Array[B]] = {
//    flatten(arr.length)(i => f(arr.at(i)))
//  }
  def flatMapArray[A: Manifest, B: Manifest](arr: Exp[Array[A]], f: Rep[A] => Rep[Array[B]]): Exp[Array[B]] = {
    val buf = ArrayBuilder.make[B]
    for (x <- arr; y <- f(x)) {
      buf += y
    }
    buf.result
  }

  def findArray[A: Manifest](a: Exp[Array[A]], f: Rep[A] => Rep[Boolean]): Exp[Array[Int]] = {
    arrayIf(a.length) { i => (f(a.at(i)), i)}
  }

  def filterArray[A: Manifest](a: Exp[Array[A]], f: Rep[A] => Rep[Boolean]): Exp[Array[A]] = {
    arrayIf(a.length) { i => { val item = a.at(i); (f(item), item) }}
  }

  def countArray[A: Manifest](a: Exp[Array[A]], f: Rep[A] => Rep[Boolean]): Exp[Int] = {
    sumIfInt(a.length)(i => (f(a.at(i)), 1))
  }
//  def countArray[A: Manifest](a: Exp[Array[A]], f: Rep[A] => Rep[Boolean]): Exp[Int] = {
//    var count = 0
//    for (x <- a) {
//      if (f(x)) count += 1
//    }
//    count
//  }

  def arrayReplicate[A: Manifest](length: Exp[Int], v: Exp[A]): Exp[Array[A]] = {
    array(length)(i => v)
  }

  def indexRangeArray(length: Exp[Int]): Exp[Array[Int]] = {
    array(length)(i => i)
  }

  def newArray[A: Manifest](length: Rep[Int]): Rep[Array[A]] = NewArray[A](length)

  def arrayZipWith[A: Manifest, B: Manifest, R: Manifest](f: (Rep[A], Rep[B]) => Rep[R], a: Exp[Array[A]], b: Exp[Array[B]]): Exp[Array[R]] = {
    array(a.length)(i => f(a.at(i), b.at(i)))
  }

  def arrayZip[A: Manifest, B: Manifest](a: Exp[Array[A]], b: Exp[Array[B]]): Exp[Array[(A, B)]] = {
    array[(A, B)](a.length)(i => (a.at(i), b.at(i)))
  }

  def arraySort[A: Manifest](a: Exp[Array[A]]): Exp[Array[A]] = {
    a.sort
  }

  def arrayReverse[A: Manifest](a: Exp[Array[A]]): Exp[Array[A]] = {
    a.reverse
  }

  def strideArray[A: Manifest](xs: Exp[Array[A]], start: Exp[Int], length: Exp[Int], stride: Exp[Int]) =
    array(length) { i =>
      xs.at(start + i * stride)
    }

  def updateArray[A: Manifest](xs: Exp[Array[A]], index: Exp[Int], value: Exp[A]) = {
    val newArr =  xs.mutable
    newArr.update(index, value)
    newArr
  }
//  def updateArray[A: Manifest](xs: Exp[Array[A]], index: Exp[Int], value: Exp[A]) = {
//    val newArr =  array_obj_new(xs.length)
//    array_copy(xs, 0, newArr, 0, xs.length)
//    newArr.update(index, value)
//    newArr
//
////    //inplace update of immutable array...
////    xs.update(index, value)
////    xs
//  }

  def arraySum[A: Manifest](xs: Exp[Array[A]])(implicit n: Numeric[A]): Exp[A] = {
    var sum = n.zero
    for (x <- xs) sum += x
    sum
  }

  def arrayMax[A: Manifest](xs: Exp[Array[A]])(implicit o: Ordering[A]): Exp[A] = {
    var max = xs.at(0) // we need Optional type to correctly implement min/max, but it is abselnt in CE
    for (x <- xs) if (x > max) max = x
    max
  }

  def arrayMin[A: Manifest](xs: Exp[Array[A]])(implicit o: Ordering[A]): Exp[A] = {
    var min = xs.at(0) // we need Optional type to correctly implement min/max, but it is abselnt in CE
    for (x <- xs) if (x < min) min = x
    min
  }

  def arrayAvg[A: Manifest](xs: Exp[Array[A]])(implicit n: Numeric[A]): Exp[Double] = {
    var sum = n.zero
    for (x <- xs) sum += x
    sum.AsInstanceOf[Double] / xs.length
  }


  def sumArray[A: Manifest](a: Exp[Array[A]]): Exp[A] = {
    sum(a.length) { i => a.at(i).AsInstanceOf[Double]}.AsInstanceOf[A]
  }

  def reduceArray[A: Manifest](a: Exp[Array[A]], zero: Exp[A], accumulate: Rep[(A, A)] => Rep[A]): Exp[A] = {
    var state = zero
    for (x <- a) {
      state = accumulate((state.AsInstanceOf[A], x))
    }
    state
  }

  /* This is not always woking */
  def scanArray[A: Manifest](a: Exp[Array[A]], zero: Exp[A], accumulate: Rep[(A, A)] => Rep[A]): Exp[(Array[A], A)] = {
    var sum = zero
    val len = a.length
    val res = newArray(len).mutable
    for (i <- 0 until len) {
      res.update(i, sum)
      sum += a.at(i)
    }
    (res, sum: Rep[A])
  }
//  def scanArray[A: Manifest](a: Exp[Array[A]], zero: Exp[A], accumulate: Rep[(A, A)] => Rep[A]): Exp[(Array[A], A)] = {
//    var state = zero
//    val arr1 = array(a.length)(i => {
//      val res = state
//      val loc = if (i==0) zero else a.at(i-1)
//      state = accumulate((state.AsInstanceOf[A], loc))
//      res
//    })
//    Tuple2(arr1, accumulate((arr1.at(a.length - 1), a.at(a.length - 1))))
//  }

  def foldArray[A: Manifest, S: Manifest](a: Exp[Array[A]], init: Exp[S], func: Rep[(S, A)] => Rep[S]): Exp[S] = {
    var state = init
    for (x <- a) {
      state = func((state.AsInstanceOf[S], x))
    }
    state
  }

  def sumArrayBy[A: Manifest, S: Manifest](a: Exp[Array[A]], func: Rep[A] => Rep[S])(implicit n: Numeric[S]): Exp[S] = {
    var sum = n.zero
    for (x <- a) {
      sum += func(x)
    }
    sum
  }

  def array_append[A: Manifest](xs: Rep[Array[A]], value: Rep[A]): Rep[Array[A]] = {
    ArrayAppend(xs, value)
  }

  case class ArrayAppend[A](xs: Rep[Array[A]], value: Rep[A])(implicit val m: Manifest[A]) extends Def[Array[A]]

  def array_cons[A: Manifest](value: Rep[A], xs: Rep[Array[A]]): Rep[Array[A]] = {
    xs.insert(0, value)
  }

  def arrayToList[A: Manifest](xs: Rep[Array[A]]): Rep[List[A]] =
    list_fromseq(array_toseq(xs))

  override def mirrorDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = {
    (e match {
      case ArrayToSeq(arr) =>
        ArrayToSeq(f(arr))
      case ArrayIndex(arr, i) =>
        ArrayIndex(f(arr), f(i))
      case a @ ArrayAppend(arr, v) =>
        ArrayAppend(f(arr), f(v))(a.m)
      case _ =>
        super.mirrorDef(e,f)
    }).asInstanceOf[Def[A]]
  }
}

trait ScalaGenArrayOpsExt extends ScalaGenBase {
  val IR: ArrayOpsExtExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case a @ ArrayAppend(xs, v) =>
      gen"""val $sym = {
           |  val len = $xs.length
           |  val d = new Array[${remap(a.m)}](len + 1)
           |  System.arraycopy($xs, 0, d, 0, len)
           |  d(len) = $v
           |  d
           |}"""
    case _ => super.emitNode(sym, rhs)
  }
}

trait CxxShptrGenArrayOpsExt extends CxxShptrCodegen {
  val IR: ArrayOpsExtExp with ArrayOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case a @ ArrayAppend(xs, v) =>
/////////////////////////////////////////////////////
// Creates new array, copies values to it from xs and adds new element
      emitNode(sym, ArrayNew(Const(0)))
      val xsLen = src"${xs}_len"
      gen"""size_t $xsLen = $xs->size();
           |$sym->resize($xsLen + 1);
           |std::copy($xs->begin(), $xs->end(), $sym->begin());
           |(*$sym)[$xsLen] = $v;"""
////////////////////////////////////////////////////
// Modifies xs adding new element and assigns new symbol to xs
//      gen"""$xs->push_back($v);"""
//      emitValDef(sym, src"$xs")
    case a @ ArrayInsert(xs,i,y) =>
      emitNode(sym, ArrayNew(Const(0)))
      val xsLen = src"${xs}_len"
      gen"""size_t $xsLen = $xs->size();
           |$sym->resize($xsLen + 1);
           |std::copy($xs->begin(), $xs->begin() + $i, $sym->begin());
           |(*$sym)[$i] = $y;
           |std::copy($xs->begin() + $i, $xs->end(), $sym->begin() + $i + 1);"""
    case _ =>
      super.emitNode(sym, rhs)
  }
}
