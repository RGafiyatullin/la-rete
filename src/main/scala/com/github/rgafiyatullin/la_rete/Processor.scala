package com.github.rgafiyatullin.la_rete

object Processor {
  type Rule[In, V] = (Filter[In, _], V)
  trait Factory {
    def createProcessor[In, V](rules: Seq[Rule[In, V]]): Processor[In, V]
  }
}

trait Processor[-In, +V] {
  def process(item: In): Option[V]
}
