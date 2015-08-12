package scalan.collections

import java.lang.reflect.Method

import scala.language.reflectiveCalls
import scalan.compilation.DummyCompilerWithPasses
import scalan.{ScalanCtxExp, ScalanCommunityDslExp, ScalanCommunityDsl, BaseCtxTests}

class SSListsTests extends BaseCtxTests {

  trait SSListsSimple extends ScalanCommunityDsl with SSListsDsl { self: ScalanCommunityDsl with SSListsDsl =>

    object sslistsTests {
      lazy val empty = fun { ignored: Rep[Int] => SSList.empty[Int].wrappedValueOfBaseType }
      lazy val map = fun { (in: Rep[Int]) => SSList.empty[Int].map({ a: Rep[Int] => a + 1 }).wrappedValueOfBaseType }
    }
  }

  test("basicTests") {
    val ctx = new TestCompilerContext {
      val compiler = new DummyCompilerWithPasses( new ScalanCommunityDslExp with SSListsSimple )
      import compiler.scalan._

      def test1() = {
        test("empty", sslistsTests.empty)
        test("map", sslistsTests.map)
      }
    }

    ctx.test1
  }
}
