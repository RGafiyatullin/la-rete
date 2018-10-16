package com.github.rgafiyatullin.la_rete.processors.trie

import com.github.rgafiyatullin.la_rete.{Node, Property}

import scala.annotation.tailrec

final class ReorderCellsInEachRow[V]
  (cardinalities: Map[Property.Untyped, Int] = Map.empty)
  extends MatrixTransformation[V]
{
  type M = MatrixTypes[V]#Matrix
  type R = MatrixTypes[V]#Row

  def reorder(nodes: Seq[Node]): Seq[Node] =
    nodes
      .sortBy { node => cardinalities.getOrElse(node.property, 0) }
      .reverse

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


  def withCardinalities(v: Map[Property.Untyped, Int]): ReorderCellsInEachRow[V] =
    new ReorderCellsInEachRow[V](v)

  override def apply(matrix0: M): M =
    matrix0.map {
      case (row, value) =>
        (processRow(row), value)
    }
}
