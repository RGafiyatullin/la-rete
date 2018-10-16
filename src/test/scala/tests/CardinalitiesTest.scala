package tests

import com.github.rgafiyatullin.la_rete.Processor.Rule
import com.github.rgafiyatullin.la_rete.processors.trie.TrieProcessor
import com.github.rgafiyatullin.la_rete.{Filter, Property}

import scala.annotation.tailrec

final class CardinalitiesTest extends TestBase {
  type V = Int
  type P = Property[V, V]

  def p(modulo: V): P =
    new Property[V, V] {
      override def apply(obj: V): V = obj % modulo
      override def isDefinedAt(obj: Any): Boolean = obj.isInstanceOf[V]
      override def toString: String = s"m$modulo"
    }

  def pAndValues(modulo: V): (P, Seq[V]) =
    (p(modulo), 0 until modulo)


  def propsWithValues(n: V): Seq[(P, Seq[V])] =
    for (i <- 1 to n) yield pAndValues(i)

  @tailrec
  def loop(acc: Seq[Filter[V, V]], ps: Seq[(P, Seq[V])]): Seq[Filter[V, V]] = {
    val (p, vs) = ps.head
    val tail = ps.tail

    val accNext =
      acc.flatMap { prev =>
        for { v <- vs }
          yield prev and Filter.property(p, v)
      }

    if (tail.isEmpty) accNext
    else loop(accNext, tail)
  }

  def rules(n: V): Seq[Rule[V, V]] =
    loop(Seq(Filter.constTrue[V]), propsWithValues(n))
      .map(f => f -> f.hashCode())

  "rules" should "work" in {
    val ruleset = rules(6)
    val processor = TrieProcessor.createProcessor(ruleset)
  }
}
