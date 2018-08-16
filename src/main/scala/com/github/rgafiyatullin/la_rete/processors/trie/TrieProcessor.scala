package com.github.rgafiyatullin.la_rete.processors.trie

import com.github.rgafiyatullin.la_rete.{Filter, Processor}

case object TrieProcessor extends Processor.Factory {
  override def createProcessor[In, V]
    (rules: Seq[(Filter[In, _], V)])
  : Processor[In, V] =
    TrieProcessor(rules)
}

final case class TrieProcessor[In, V](rules: Seq[Processor.Rule[In, V]]) extends Processor[In, V] {
  private type Row = MatrixTypes[V]#Row
  private type Matrix = MatrixTypes[V]#Matrix

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
    util.dumpMatrix("M0", m0)
    val m1 = reorderCellsInEachRow(m0)
    util.dumpMatrix("M1", m1)
    val trie = intoTrie(m1)
    println("==== BEGIN TRIE ====")
    trie.dump.foreach(println)
    println("====  END  TRIE ====")
    trie
  }

  override def process(item: In): Option[V] = trie(item)
}
