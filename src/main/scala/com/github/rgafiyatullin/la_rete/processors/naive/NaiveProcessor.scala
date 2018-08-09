package com.github.rgafiyatullin.la_rete.processors.naive

import com.github.rgafiyatullin.la_rete.{Node, Processor}

object NaiveProcessor extends Processor.Factory {
  override def createProcessor[In, V]
    (rules: Seq[Processor.Rule[In, V]])
  : Processor[In, V] =
    NaiveProcessor(rules)
}

final case class NaiveProcessor[In, V] private (
  rules: Seq[Processor.Rule[In, V]])
    extends Processor[In, V]
{
  private type Row[V] = (Seq[Node], V)

  private lazy val matrix: Seq[Row[V]] =
    rules
      .map { case (filter, value) => filter.nodes.map(_ -> value) }
      .reduce(_ ++ _)

  private def check(nodes: Seq[Node], item: In): Boolean =
    nodes
      .forall { node =>
        println(s"  Node: $node; extract: ${node.property.extract(item)}; value: ${node.value}")
        node.property.extract(item).contains(node.value) }

  override def process(item: In): Option[V] = {
    println(s"Item: $item")
    matrix
      .find { case (nodes, _) =>
        println(s"Trying: $nodes")
        check(nodes, item)
      }
      .map(_._2)
  }
}
