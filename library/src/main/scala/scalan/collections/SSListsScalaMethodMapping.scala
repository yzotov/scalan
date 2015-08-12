package scalan.collections

import scalan.{CommunityMethodMappingDSL, ScalanCommunityDslExp}

trait SSListsScalaMethodMapping extends CommunityMethodMappingDSL { self: { val scalan: ScalanCommunityDslExp } =>
  import scala.language.reflectiveCalls
  import scala.reflect.runtime.universe._
  import scalan._

  new ScalaMappingDSL with MappingTags {

    val scalan_collections_SSList = {
      val sseqClass = findDefinition(SSList) match {
        case Some(TableEntry(sym, rhs)) =>
          rhs.getClass
      }

      new ClassType(Symbol(sseqClass.getName)) {
        val apply = Method('apply, typeOf[List[_]], MethodArg(typeOf[Array[_]]))
        val empty = Method('empty, typeOf[List[_]])
      }
    }

    val scala_collection_List = new ScalaLib() {
      val arrayToList = ScalaFunc(Symbol("(new AnyRef {def apply[T](arr: Array[T]):List[T] = arr.toList})"))(false)
      val empty = ScalaFunc(Symbol("List.empty"))(false)
    }

    val mapping = new ScalaMapping {
      val functionMap = Map( scalan_collections_SSList.apply -> scala_collection_List.arrayToList
        , scalan_collections_SSList.empty -> scala_collection_List.empty)
    }
  }
}
