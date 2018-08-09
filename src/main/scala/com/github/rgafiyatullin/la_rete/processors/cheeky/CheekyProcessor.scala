package com.github.rgafiyatullin.la_rete.processors.cheeky

import com.github.rgafiyatullin.la_rete.{Filter, Processor}

case object CheekyProcessor extends Processor.Factory {
  override def createProcessor[In, V]
    (rules: Seq[(Filter[In, _], V)])
  : Processor[In, V] =
    CheekyProcessor(rules)
}

final case class CheekyProcessor[In, V](rules: Seq[Processor.Rule[In, V]]) extends Processor[In, V] {
  private type Row = CheekyTypes[V]#Row
  private type Matrix = CheekyTypes[V]#Matrix

  private def initialMatrix: Seq[Row] =
    rules
      .map { case (filter, value) => filter.nodes.map(_ -> value) }
      .reduce(_ ++ _)

  private val reorderCellsInEachRow = new ReorderCellsInEachRow[V]
  private val intoTrie = new IntoTrie[V]

  private object util {
    def dumpMatrix(name: String, matrix: Matrix): Unit = {
      println(s"==== BEGIN [$name] ====")
      for {
        (row, v) <- matrix
      } {
        for { cell <- row } print("%s ".format(cell).padTo(30, ' '))
        println(s" => $v")
      }
      println(s"====  END  [$name] ====")
    }
  }

  private val trie = {
    val m0 = initialMatrix
//    util.dumpMatrix("M0", m0)
    val m1 = reorderCellsInEachRow(m0)
//    util.dumpMatrix("M1", m1)
    val trie = intoTrie(m1)
//    println("==== BEGIN TRIE ====")
//    trie.dump.foreach(println)
//    println("====  END  TRIE ====")
    trie
  }

  override def process(item: In): Option[V] = trie(item)
}
