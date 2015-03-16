package scalan.it.lms

import scalan.ScalanCommunityDslExp
import scalan.compilation.GraphVizConfig
import scalan.compilation.lms.cxx.sharedptr.CoreCxxShptrLmsBackend
import scalan.compilation.lms.cxx.{CoreCXXLmsBackend, LmsCompilerCXX}
import scalan.util.FileUtil
import scalan.{ScalanCtxSeq, ScalanCtxExp}
import scalan.compilation.lms._
import scalan.compilation.lms.scalac.{CommunityLmsCompilerScala, LmsCompilerScala}
import scalan.graphs.{GraphsDslExp, GraphsDslSeq, GraphExamples, MST_example}
import scalan.it.BaseItTests


abstract class LmsMstItTests extends BaseItTests {
  class ProgExp extends MST_example with ScalanCommunityDslExp with CommunityLmsCompilerScala with CommunityBridge { self =>
    val lms = new CommunityLmsBackend
  }

  class ProgExpCXX extends MST_example with ScalanCommunityDslExp with LmsCompilerCXX with CoreBridge { self =>
    val lms = new CoreCxxShptrLmsBackend
  }

  class ProgDslExp extends GraphsDslExp with GraphExamples with ScalanCommunityDslExp with CommunityLmsCompilerScala with CommunityBridge { self =>
    val lms = new CommunityLmsBackend
  }
  class ProgDslSeq extends GraphsDslSeq with GraphExamples with ScalanCtxSeq
  class ProgSeq extends MST_example with ScalanCtxSeq

  val progStaged = new ProgExp
  val progStagedCXX = new ProgExpCXX
  val progSeq = new ProgSeq
  val progDslStaged = new ProgDslExp
  val progDslSeq = new ProgDslSeq

  def sparseVectorData(arr: Array[Double]) = (0.until(arr.length).toArray, (arr, arr.length))
}

class LmsMstPrimeItTests extends LmsMstItTests {
  import progSeq._
  val graph = Array(
    Array(1, 8),
    Array(0, 2, 8),
    Array(1, 3, 5, 7),
    Array(2, 4, 5),
    Array(3, 5),
    Array(2, 3, 4, 6),
    Array(5, 7, 8),
    Array(2, 6, 8),
    Array(0, 1, 6, 7),
    Array(10,11),
    Array(9,11),
    Array(9,10)
  )
  val graphValues = Array(
    Array(4.0, 8.0),
    Array(4.0, 8.0, 11.0),
    Array(8.0, 7.0, 4.0, 2.0),
    Array(7.0, 9.0, 14.0),
    Array(9.0, 10.0),
    Array(4.0, 14.0, 10.0, 2.0),
    Array(2.0, 6.0, 1.0),
    Array(2.0, 6.0, 7.0),
    Array(8.0, 11.0, 1.0, 7.0),
    Array(1.0, 2.0),
    Array(1.0, 0.5),
    Array(2.0, 0.5)
  )

  test("MST_adjList") {

    val links = graph.flatMap( i=> i)
    val edgeVals = graphValues.flatMap(i => i)
    val lens = graph.map(i => i.length)
    val offs = Array(0,2,5,9,12,14,18,21,24,28,30,32) //(Array(0) :+ lens.scan.slice(lens.length-1)
    val input = (links, (edgeVals, (offs, lens)))
    val res = progSeq.MST_adjlist(input)
    compareOutputWithSequential(progStaged)(progSeq.MST_adjlist, progStaged.MST_adjlist, "MST_adjList", input)
    val dir = FileUtil.file(prefix, "MST_adjList")
    progStagedCXX.buildExecutable(dir,dir,"MST_adjList", progStagedCXX.MST_adjlist, GraphVizConfig.default)(progStagedCXX.defaultCompilerConfig)
    println(res.mkString(" , "))
  }
  test("MSF_adjList") {

    val links = graph.flatMap( i=> i)
    val edgeVals = graphValues.flatMap(i => i)
    val lens = graph.map(i => i.length)
    val offs = Array(0,2,5,9,12,14,18,21,24,28,30,32) //(Array(0) :+ lens.scan.slice(lens.length-1)
    val input = (links, (edgeVals, (offs, lens)))
    val res = progSeq.MSF_adjlist(input)
    //compareOutputWithSequential(progStaged)(progSeq.MST, progStaged.MST, "MST_adjList", input)
    val dir = FileUtil.file(prefix, "MSF_adjlist")
    progStagedCXX.buildExecutable(dir,dir,"MSF_adjlist", progStagedCXX.MSF_adjlist, GraphVizConfig.default)(progStagedCXX.defaultCompilerConfig)
    println(res.mkString(" , "))
  }

  test("MST_adjMatrix") {
    val vertexNum = graph.length
    val incMatrix = (graph zip graphValues).flatMap({ in =>
      val row = in._1
      val vals = in._2
      val zero = scala.Array.fill(vertexNum)(0.0)
      for (i <- 0 to row.length-1) { zero(row(i)) = vals(i) }
      zero
    })
    val input = (incMatrix, vertexNum)
    val res = progSeq.MST_adjmatrix(input)
    compareOutputWithSequential(progStaged)(progSeq.MST_adjmatrix, progStaged.MST_adjmatrix, "MST_adjMatrix", input)
    val dir = FileUtil.file(prefix, "MST_adjMatrix")
    progStagedCXX.buildExecutable(dir,dir,"MST_adjMatrix", progStagedCXX.MST_adjmatrix, GraphVizConfig.default)(progStagedCXX.defaultCompilerConfig)
    println(res.mkString(" , "))
  }
  test("MSF_adjMatrix") {
    val vertexNum = graph.length
    val incMatrix = (graph zip graphValues).flatMap({ in =>
      val row = in._1
      val vals = in._2
      val zero = scala.Array.fill(vertexNum)(0.0)
      for (i <- 0 to row.length-1) { zero(row(i)) = vals(i) }
      zero
    })
    val input = (incMatrix, vertexNum)
    val res = progSeq.MSF_adjmatrix(input)
    //compareOutputWithSequential(progStaged)(progSeq.MST, progStaged.MST, "MST_adjMatrix", input)
    val dir = FileUtil.file(prefix, "MSF_adjmatrix")
    progStagedCXX.buildExecutable(dir,dir,"MSF_adjmatrix", progStagedCXX.MSF_adjmatrix, GraphVizConfig.default)(progStagedCXX.defaultCompilerConfig)
    println(res.mkString(" , "))
  }

  test("MSF_adjListMap") {
    val links = graph.flatMap( i=> i)
    val edgeVals = graphValues.flatMap(i => i)
    val lens = graph.map(i => i.length)
    val offs = Array(0,2,5,9,12,14,18,21,24,28,30,32) //(Array(0) :+ lens.scan.slice(lens.length-1)
    val input = (links, (edgeVals, (offs, lens)))
    val resSeq = progSeq.MSF_adjlistMap(input)
    println(resSeq.mkString(" , "))
    val resStaged = getStagedOutputConfig(progStaged)(progStaged.MSF_adjlistMap, "MSF_adjlistMap", input, progStaged.defaultCompilerConfig)
    println("Staged: " + resStaged.mkString(","))
//    val dir = FileUtil.file(prefix, "MSF_adjlistMap")
//    progStagedCXX.buildExecutable(dir,dir,"MSF_adjlistMap", progStagedCXX.MSF_adjlistMap, GraphVizConfig.default)(progStagedCXX.defaultCompilerConfig)
  }

  test("MSF_adjMatrixMap") {
    pending
    val vertexNum = graph.length
    val incMatrix = (graph zip graphValues).flatMap({ in =>
      val row = in._1
      val vals = in._2
      val zero = scala.Array.fill(vertexNum)(0.0)
      for (i <- 0 to row.length-1) { zero(row(i)) = vals(i) }
      zero
    })
    val input = (incMatrix, vertexNum)
    val resSeq = progSeq.MSF_adjmatrixMap(input)
    println(resSeq.mkString(" , "))
    val resStaged = getStagedOutputConfig(progStaged)(progStaged.MSF_adjmatrixMap, "MSF_adjmatrixMap", input, progStaged.defaultCompilerConfig)
    println("Staged: " + resStaged.mkString(","))
  }
  test("MST_adjListMap") {
    val links = graph.flatMap( i=> i)
    val edgeVals = graphValues.flatMap(i => i)
    val lens = graph.map(i => i.length)
    val offs = Array(0,2,5,9,12,14,18,21,24,28,30,32) //(Array(0) :+ lens.scan.slice(lens.length-1)
    val input = (links, (edgeVals, (offs, lens)))
    val resSeq = progSeq.MST_adjlistMap(input)
    println(resSeq.mkString(" , "))
    val resStaged = getStagedOutputConfig(progStaged)(progStaged.MST_adjlistMap, "MST_adjlistMap", input, progStaged.defaultCompilerConfig)
    println("Staged: " + resStaged.mkString(","))
//    val dir = FileUtil.file(prefix, "MST_adjlistMap")
//    progStagedCXX.buildExecutable(dir,dir,"MST_adjlistMap", progStagedCXX.MST_adjlistMap, GraphVizConfig.default)(progStagedCXX.defaultCompilerConfig)
  }
  test("MST_adjMatrixMap") {
    pending
    val vertexNum = graph.length
    val incMatrix = (graph zip graphValues).flatMap({ in =>
      val row = in._1
      val vals = in._2
      val zero = scala.Array.fill(vertexNum)(0.0)
      for (i <- 0 to row.length-1) { zero(row(i)) = vals(i) }
      zero
    })
    val input = (incMatrix, vertexNum)
    val resSeq = progSeq.MST_adjmatrixMap(input)
    println(resSeq.mkString(" , "))
    val resStaged = getStagedOutputConfig(progStaged)(progStaged.MST_adjmatrixMap, "MST_adjmatrixMap", input, progStaged.defaultCompilerConfig)
    println("Staged: " + resStaged.mkString(","))
  }

  test("MSF_adjListList") {
//    pending
    val links = graph.flatMap( i=> i)
    val edgeVals = graphValues.flatMap(i => i)
    val lens = graph.map(i => i.length)
    val offs = Array(0,2,5,9,12,14,18,21,24,28,30,32) //(Array(0) :+ lens.scan.slice(lens.length-1)
    val input = (links, (edgeVals, (offs, lens)))
    val resSeq = progSeq.MSF_adjlistList(input)
    println(resSeq.mkString(" , "))
    val resStaged = getStagedOutputConfig(progStaged)(progStaged.MSF_adjlistList, "MSF_adjlistList", input, progStaged.defaultCompilerConfig)
    println("Staged: " + resStaged.mkString(","))
    val dir = FileUtil.file(prefix, "MSF_adjlistList")
    progStagedCXX.buildExecutable(dir,dir,"MSF_adjlistList", progStagedCXX.MSF_adjlistList, GraphVizConfig.default)(progStagedCXX.defaultCompilerConfig)
  }

  test("MSF_adjMatrixList") {
//    pending
    val vertexNum = graph.length
    val incMatrix = (graph zip graphValues).flatMap({ in =>
      val row = in._1
      val vals = in._2
      val zero = scala.Array.fill(vertexNum)(0.0)
      for (i <- 0 to row.length-1) { zero(row(i)) = vals(i) }
      zero
    })
    val input = (incMatrix, vertexNum)
    val resSeq = progSeq.MSF_adjmatrixList(input)
    println(resSeq.mkString(" , "))
    val resStaged = getStagedOutputConfig(progStaged)(progStaged.MSF_adjmatrixList, "MSF_adjmatrixList", input, progStaged.defaultCompilerConfig)
    println("Staged: " + resStaged.mkString(","))
    val dir = FileUtil.file(prefix, "MSF_adjmatrixList")
    progStagedCXX.buildExecutable(dir,dir,"MSF_adjmatrixList", progStagedCXX.MSF_adjmatrixList, GraphVizConfig.default)(progStagedCXX.defaultCompilerConfig)
  }

  test("MST_adjMatrixList") {
    pending
    val vertexNum = graph.length
    val incMatrix = (graph zip graphValues).flatMap({ in =>
      val row = in._1
      val vals = in._2
      val zero = scala.Array.fill(vertexNum)(0.0)
      for (i <- 0 to row.length-1) { zero(row(i)) = vals(i) }
      zero
    })
    val input = (incMatrix, vertexNum)
    val resSeq = progSeq.MST_adjmatrixList(input)
    println(resSeq.mkString(" , "))
    val resStaged = getStagedOutputConfig(progStaged)(progStaged.MST_adjmatrixList, "MST_adjmatrixList", input, progStaged.defaultCompilerConfig)
    println("Staged: " + resStaged.mkString(","))
  }

}
