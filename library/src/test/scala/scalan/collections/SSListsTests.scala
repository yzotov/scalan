package scalan.collections

import java.lang.reflect.Method

import scala.language.reflectiveCalls
import scalan.{ScalanCommunityDslExp, ScalanCommunityDsl, BaseCtxTests}

class SSListsTests extends BaseCtxTests {

  trait SSListsSimple extends ScalanCommunityDsl with SSListsDsl { self: ScalanCommunityDsl with SSListsDsl =>

    lazy val empty = SSList.empty[Int].wrappedValueOfBaseType
    lazy val map = fun { (in: Rep[Int]) => SSList.empty[Int].map({a: Rep[Int] => a + 1}).wrappedValueOfBaseType }
  }

  test("basicTests") {
    val ctx = new TestCompilerContext with SSListsSimple with ScalanCommunityDslExp with SSListsDslExp {
      override def shouldUnpack(e: Elem[_]) = true
      override def isInvokeEnabled(d: Def[_], m: Method) = true
      def test() = { }
    }
    ctx.test
    ctx.emit("empty", ctx.empty)
    ctx.emit("map", ctx.map)
  }
}
