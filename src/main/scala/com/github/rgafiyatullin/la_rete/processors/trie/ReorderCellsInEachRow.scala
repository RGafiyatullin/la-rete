package com.github.rgafiyatullin.la_rete.processors.trie

import com.github.rgafiyatullin.la_rete.Node

import scala.annotation.tailrec

final class ReorderCellsInEachRow[V] extends MatrixTransformation[V] {
  type M = MatrixTypes[V]#Matrix
  type R = MatrixTypes[V]#Row

  def reorder(nodes: Seq[Node]): Seq[Node] =
    nodes.sortBy(_.property.toString)

  @tailrec
  def loop(acc: Seq[Node], input: Seq[Node]): Seq[Node] =
    (input.takeWhile(!_.changeOfType), input.dropWhile(!_.changeOfType)) match {
      case (Seq(), Seq()) =>
        acc
      case (nodesToReorder, remainder) =>
        loop(acc ++ reorder(nodesToReorder) ++ remainder.take(1), remainder.drop(1))
    }

  def processRow(nodes: Seq[Node]): Seq[Node] =
    loop(Seq.empty, nodes)


  override def apply(matrix0: M): M =
    matrix0.map {
      case (row, value) =>
        (processRow(row), value)
    }
}
