package com.github.rgafiyatullin.la_rete.processors.cheeky

import com.github.rgafiyatullin.la_rete.Property

object IntoTrie {
  sealed trait Trie[+V] extends Function[Any, Option[V]] {
    def dump: Seq[String] =
      dumpRecursively(0)

    def dumpRecursively(depth: Int): Seq[String]
  }
  object Trie {
    final case class WithChildren[+V]
      (property: Property.Untyped,
       children: Map[Any, Trie[V]],
       fallback: Trie[V]) extends Trie[V]
    {
      override def apply(item: Any): Option[V] =
        property
          .extract(item).flatMap(children.get).flatMap(_ apply item)
          .orElse(fallback(item))

      override def dumpRecursively(depth: Int): Seq[String] =
        Seq("".padTo(depth, '|') + s"WithChildren($property)") ++
          children.toList.flatMap {
            case (k, child) =>
              ("".padTo(depth, '|') + "-[" + k + "] =>") +: child.dumpRecursively(depth + 2)
          } ++
          fallback.dumpRecursively(depth + 1)
    }

    final case class Constant[+V](value: V) extends Trie[V] {
      override def apply(item: Any): Option[V] = Some(value)

      override def dumpRecursively(depth: Int): Seq[String] =
        Seq("".padTo(depth, '|') + s"Constant($value)")
    }

    case object DeadEnd extends Trie[Nothing] {
      override def apply(item: Any): Option[Nothing] = None

      override def dumpRecursively(depth: Int): Seq[String] =
        Seq("".padTo(depth, '|') + "DeadEnd")
    }
  }
}

final class IntoTrie[V] extends Function[CheekyTypes[V]#Matrix, IntoTrie.Trie[V]] {
  type M = CheekyTypes[V]#Matrix
  type R = CheekyTypes[V]#Row

  import IntoTrie.Trie

  def buildTrie(matrix: M): IntoTrie.Trie[V] =
    matrix match {
      case (Seq(), value) +: rows =>
        assert(rows.isEmpty)
//        println(s"case#1: value=$value")
        Trie.Constant(value)

      case (thisRow @ (node +: nodes, value)) +: rows =>
        val leftGroup = thisRow +: rows.takeWhile(_._1.headOption.exists(_.property == node.property))
        val rightGroup = rows.dropWhile(_._1.headOption.exists(_.property == node.property))

//        println(s"case#2: " +
//          s"left=${leftGroup.size}; " +
//          s"right=${rightGroup.size}; " +
//          s"keys=${leftGroup.map(_._1.head.value).toSet}; " +
//          s"${node.property} ~ ${rows.map(_._1.headOption.map(n => (n.property, n.property == node.property)).get)}")

        val children =
          leftGroup
            .groupBy(_._1.head.value)
            .map { case (childKey, rows: Seq[R]) =>
              childKey -> buildTrie(
                rows.collect { case (_ :: childPath, childValue) => childPath -> childValue }) }



        Trie.WithChildren(node.property, children, buildTrie(rightGroup))

      case Seq() =>
//        println("case#3:")
        Trie.DeadEnd
    }

  override def apply(matrix: M): IntoTrie.Trie[V] =
    buildTrie(matrix)
}
