package com.github.rgafiyatullin.la_rete.processors.trie

import com.github.rgafiyatullin.la_rete.Property

final class CalculatePropertiesCardinalities {
  type M = MatrixTypes[Any]#Matrix
  type R = MatrixTypes[Any]#Row

  def apply(m: M): Map[Property.Untyped, Int] =
    m
      .flatMap(_._1)
      .map { n => n.property -> n.value }
      .groupBy(_._1)
      .mapValues(_.distinct.size)
      .toSeq
      .toMap
}
