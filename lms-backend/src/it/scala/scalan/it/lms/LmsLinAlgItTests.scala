package scalan.it.lms

import scalan._
import scalan.compilation.lms.uni._
import scalan.util.FileUtil._
import scalan.compilation.lms._
import scalan.compilation.lms.scalac.CommunityLmsCompilerScala
import scalan.it.BaseItTests
import scalan.linalgebra.{LinearAlgebraExamples, MatricesDslSeq}

abstract class LmsLinAlgItTests extends BaseItTests {
  class ProgExp extends LinearAlgebraExamples with ScalanCommunityDslExp with JNIExtractorOpsExp

  class ProgSeq extends LinearAlgebraExamples with MatricesDslSeq with ScalanCommunityDslSeq

  val progStaged = new CommunityLmsCompilerScala with CommunityBridge {
    val scalan = new ProgExp
  }

  val progStagedU = new LmsCompilerUni with CommunityBridge with CommunityMethodMappingDSL {
    val scalan = new ProgExp
  }

  val progSeq = new ProgSeq

  val compilerConfigU = progStagedU.defaultCompilerConfig.copy(scalaVersion = Some("2.11.2"))
  
  def sparseVectorData(arr: Array[Double]) = (0.until(arr.length).toArray, (arr, arr.length))
}

class LmsMvmItTests extends LmsLinAlgItTests {
  import progSeq._

  test("ddmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged)(progSeq.ddmvm, progStaged.scalan.ddmvm, "ddmvm", in)
    compareOutputWithSequentialConfig(progStagedU)(progSeq.ddmvm, progStagedU.scalan.ddmvm, "ddmvm", in, compilerConfigU)
  }

  test("ddmvmList(scala)") {
    val inM = List(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged)(progSeq.ddmvmList, progStaged.scalan.ddmvmList, "ddmvmList", in)
  }

  test("dsmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged)(progSeq.dsmvm, progStaged.scalan.dsmvm, "dsmvm", in)
    compareOutputWithSequential(progStagedU)(progSeq.dsmvm, progStagedU.scalan.dsmvm, "dsmvm", in)
  }

  test("sdmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged)(progSeq.sdmvm, progStaged.scalan.sdmvm, "sdmvm", in)
    compareOutputWithSequential(progStagedU)(progSeq.sdmvm, progStagedU.scalan.sdmvm, "sdmvm", in)
  }

  test("ssmvm") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged)(progSeq.ssmvm, progStaged.scalan.ssmvm, "ssmvm", in)
    compareOutputWithSequential(progStagedU)(progSeq.ssmvm, progStagedU.scalan.ssmvm, "ssmvm", in)
  }

  test("fdmvm") {
    val inM = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged)(progSeq.fdmvm, progStaged.scalan.fdmvm, "fdmvm", in)
  }

  test("fsmvm") {
    val inM = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged)(progSeq.fsmvm, progStaged.scalan.fsmvm, "fsmvm", in)
    compareOutputWithSequential(progStagedU)(progSeq.fsmvm, progStagedU.scalan.fsmvm, "fsmvm", in)
  }

  test("ddmvm0") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged)(progSeq.ddmvm0, progStaged.scalan.ddmvm0, "ddmvm0", in)
    compareOutputWithSequential(progStagedU)(progSeq.ddmvm0, progStagedU.scalan.ddmvm0, "ddmvm0", in)
  }

  test("dsmvm0") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged)(progSeq.dsmvm0, progStaged.scalan.dsmvm0, "dsmvm0", in)
    compareOutputWithSequential(progStagedU)(progSeq.dsmvm0, progStagedU.scalan.dsmvm0, "dsmvm0", in)
  }

  test("sdmvm0") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged)(progSeq.sdmvm0, progStaged.scalan.sdmvm0, "sdmvm0", in)
    compareOutputWithSequential(progStagedU)(progSeq.sdmvm0, progStagedU.scalan.sdmvm0, "sdmvm0", in)
  }

  test("ssmvm0") {
    val inM = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged)(progSeq.ssmvm0, progStaged.scalan.ssmvm0, "ssmvm0", in)
    compareOutputWithSequential(progStagedU)(progSeq.ssmvm0, progStagedU.scalan.ssmvm0, "ssmvm0", in)
  }

  test("fdmvm0") {
    val inM = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inV = Array(2.0, 3.0)
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged)(progSeq.fdmvm0, progStaged.scalan.fdmvm0, "fdmvm0", in)
    compareOutputWithSequential(progStagedU)(progSeq.fdmvm0, progStagedU.scalan.fdmvm0, "fdmvm0", in)
  }

  test("fsmvm0") {
    val inM = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inV = sparseVectorData(Array(2.0, 3.0))
    val in = Pair(inM, inV)
    val out = Array(5.0, 3.0)
    compareOutputWithSequential(progStaged)(progSeq.fsmvm0, progStaged.scalan.fsmvm0, "fsmvm0", in)
    compareOutputWithSequential(progStagedU)(progSeq.fsmvm0, progStagedU.scalan.fsmvm0, "fsmvm0", in)
  }
}

class LmsMmmItTests extends LmsLinAlgItTests {
  import progSeq._

  test("ddmmm") {
    val inM1 = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val inM2 = Array(Array(1.0, 1.0), Array(0.0, 1.0))
    val in = Pair(inM1, inM2)
    val out = Array(Array(1.0, 2.0), Array(0.0, 1.0))
    compareOutputWithSequential(progStaged)(progSeq.ddmmm, progStaged.scalan.ddmmm, "ddmmm", in)
    compareOutputWithSequential(progStagedU)(progSeq.ddmmm, progStagedU.scalan.ddmmm, "ddmmm", in)
  }

  test("ssmmm") {
    //pending
    val inM1 = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val inM2 = Array(Array(1.0, 1.0), Array(0.0, 1.0)).map(sparseVectorData)
    val in = Pair(inM1, inM2)
    val out = Array(Array(1.0, 2.0), Array(0.0, 1.0))
    compareOutputWithSequential(progStaged)(progSeq.ssmmm, progStaged.scalan.ssmmm, "ssmmm", in)
    compareOutputWithSequential(progStagedU)(progSeq.ssmmm, progStagedU.scalan.ssmmm, "ssmmm", in)
    compareOutputWithExpected(progStaged)(out, progStaged.scalan.ssmmm, "ssmmm_out", in)
    compareOutputWithExpected(progStagedU)(out, progStagedU.scalan.ssmmm, "ssmmm_out", in)
  }

  test("ffmmm") {
    val inM1 = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val inM2 = (Array(1.0, 1.0, 0.0, 1.0), 2)
    val in = Pair(inM1, inM2)
    val out = Array(Array(1.0, 2.0), Array(0.0, 1.0))
    compareOutputWithSequential(progStaged)(progSeq.ffmmm, progStaged.scalan.ffmmm, "ffmmm", in)
    compareOutputWithSequential(progStagedU)(progSeq.ffmmm, progStagedU.scalan.ffmmm, "ffmmm", in)
  }
}

class AbstractElemItTests extends LmsLinAlgItTests {
  import progSeq._

  lazy val jArrTrain2x2 = Array(Array((0, 5.0), (1, 3.0)), Array((1, 4.0)))
  lazy val jArrTest2x2 = Array(Array((0, 5.0), (1, 3.0)), Array((0, 3.0), (1, 4.0)))

  def getNArrayWithSegmentsFromJaggedArray(jaggedArray: Array[Array[(Int, Double)]]) = {
    val arr = jaggedArray.flatMap(v => v)
    val lens = jaggedArray.map(i => i.length)
    val offs = lens.scanLeft(0)((x, y) => x + y).take(lens.length)
    (arr, offs zip lens)
  }

  test("pattern matching vectors with abstract elem works") {
    val arrTrain = Array((0, 5.0), (1, 3.0), (1, 4.0))
    lazy val width = 5

    val in = Tuple(width, arrTrain)
    getStagedOutput(progStaged)(progStaged.scalan.dotWithAbstractElem, "patternMatchAbstract", in)
  }

  test("elems divergence in if_then_else branches") {
    // Different branches of if_then_else operator produce different elems.
    // This causes Sums and SumViews to appear.
    // The test verifies iso lifting in this case (see IfThenElse.rewriteDef)
    val matrix = Array(Array(0, 5), Array(1, 3), Array(1, 4))
    val vector = Array(1,2)

    val in = Tuple(matrix, vector)
    progStaged.buildGraph(file(prefix, "simpleSum"), "simpleSum", progStaged.scalan.funSimpleSum, graphVizConfig)(progStaged.defaultCompilerConfig)
  }

}

class VectorMethodsItTests extends LmsLinAlgItTests {

  import progSeq._

  lazy val vector1 = Array(Pair(0, 1.0), Pair(1, 2.0), Pair(2, 3.0), Pair(3, 4.0), Pair(4, 5.0))
  lazy val vector2 = Array(Pair(0, 1.0), Pair(7, 3.0), Pair(12, 5.0))

  test("applySparseVector1") {

    //val progStaged = new ProgExp

    lazy val len = 5
    val i = 2
    val in = Tuple(vector1, len, i)
    val out = 3
    compareOutputWithSequential(progStaged)(progSeq.applySparseVector, progStaged.scalan.applySparseVector, "applySparseVector1", in)
    compareOutputWithExpected(progStaged)(out, progStaged.scalan.applySparseVector, "applySparseVector1e", in)
  }

  test("applySparseVector2") {

    //val progStaged = new ProgExp

    lazy val len = 12
    val i = 12
    val in = Tuple(vector2, len, i)
    val out = 5
    compareOutputWithSequential(progStaged)(progSeq.applySparseVector, progStaged.scalan.applySparseVector, "applySparseVector2", in)
    compareOutputWithExpected(progStaged)(out, progStaged.scalan.applySparseVector, "applySparseVector2e", in)
  }

  test("transpose") {
    val nItems = 2
    val (arrTrain, segsTrain) = progSeq.getNArrayWithSegmentsFromJaggedArray(progSeq.jArrTrain2x2)
    val in = progSeq.Tuple(arrTrain, segsTrain, nItems)

    compareOutputWithSequential(progStaged)(progSeq.transpose, progStaged.scalan.transpose, "transpose", in)
  }

  // the below two tests are ignored because they can fail due to randomness
  // we could also just decrease the chance of this significantly
  ignore("random") {
    val c = 1.0
    val in = c

    compareOutputWithSequential(progStaged)(progSeq.funRandom, progStaged.scalan.funRandom, "random", in)
  }

  ignore("randomArray") {
    val c = 1000
    val in = c

    compareOutputWithSequential(progStaged)(progSeq.funRandomArray, progStaged.scalan.funRandomArray, "randomArray", in)
  }

  test("ZipMapViewBoth") {
    val c = 10
    val in = c

    compareOutputWithSequential(progStaged)(progSeq.funZipMapViewBoth, progStaged.scalan.funZipMapViewBoth, "ZipMapViewBoth", in)
  }

  test("ZipMapViewLeft") {
    val c = 10
    val in = c

    compareOutputWithSequential(progStaged)(progSeq.funZipMapViewLeft, progStaged.scalan.funZipMapViewLeft, "funZipMapViewLeft", in)
  }

  test("ZipMapViewRight") {
    val c = 10
    val in = c

    compareOutputWithSequential(progStaged)(progSeq.funZipMapViewRight, progStaged.scalan.funZipMapViewRight, "funZipMapViewRight", in)
  }

  test("collReplicateFilter") {
    val in = Array(1, 2, 4)
    compareOutputWithSequential(progStaged)(progSeq.collReplicateFilter, progStaged.scalan.collReplicateFilter, "collReplicateFilter", in)
  }
}
