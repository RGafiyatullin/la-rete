package com.github.rgafiyatullin.la_rete.processors.naive

import com.github.rgafiyatullin.la_rete.{Node, Processor}

case object NaiveProcessor extends Processor.Factory {
  override def createProcessor[In, V]
    (rules: Seq[Processor.Rule[In, V]])
  : Processor[In, V] =
    NaiveProcessor(rules)
}

final case class NaiveProcessor[In, V] private (
  rules: Seq[Processor.Rule[In, V]])
    extends Processor[In, V]
{
  private type Row = (Seq[Node], V)

  private lazy val matrix: Seq[Row] =
    rules
      .map { case (filter, value) => filter.nodes.map(_ -> value) }
      .reduce(_ ++ _)

  private def check(nodes: Seq[Node], item: In): Boolean =
    nodes
      .forall { node =>
        node.property.extract(item).contains(node.value) }

  override def process(item: In): Option[V] = {
    matrix
      .find { case (nodes, _) => check(nodes, item) }
      .map(_._2)
  }
}
