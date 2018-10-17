package com.github.rgafiyatullin.la_rete.processors.trie

import com.github.rgafiyatullin.la_rete.Property

object IntoTrie {

  /**
    * A Trie-node and a Trie-itself (if the node is the root node).
    * An Algebraic Data Type having three possible cases:
    * - Constant
    * - DeadEnd
    * - WithChildren
    *
    * The trie is walked from root to a leaf-node according to the matches:
    * - Constant-node — a leaf-node: take the value contained within the node (return Some(value));
    * - DeadEnd-node — a leaf-node: no value matches the input (return None);
    * - WithChildren —
    *   - take a node.property (property extractor);
    *   - use it to extract property-value from the input;
    *   - lookup a child-node associated with the latter property-value in node.children;
    *     - if found — proceed with the yielded child-node;
    *     - otherwise — proceed with the child-node contained in node.fallback.
    *
    * @tparam V
    */
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

final class IntoTrie[V] extends Function[MatrixTypes[V]#Matrix, IntoTrie.Trie[V]] {
  type M = MatrixTypes[V]#Matrix
  type R = MatrixTypes[V]#Row

  import IntoTrie.Trie

  /**
    * Build a Trie-node representing the rules in the matrix-parameter
    *
    * @param matrix
    * @return
    */
  def buildTrie(matrix: M): IntoTrie.Trie[V] =
    matrix match {
      /**
        * the head-row contains a rule having no predicates: this will always match — hence a Trie.Constant is yielded.
        * The rest of the rules are "shaded" by the current rule: those would never be tried, we throw them away.
        */
      case (Seq(), value) +: rows =>
        if (rows.nonEmpty) // TODO: a println is not a good way to indicate warnings.
          println(s"Shaded predicates: $rows")

        Trie.Constant(value)

      /**
        * A rule has at least one predicate.
        *
        * The following input matrix (the first three rules have pA as the property of their head-predicates)
        *
        * pA[=1]  p12  p13  p14 ... -> v1
        * pA[=1]  p22  p23  p24 ... -> v2
        * ......  ...  ...  ...
        * pA[=N]  p32  p33  p34 ... -> v3
        * pB      p42  p43  p44 ... -> v4
        * p51     p52  p53  p54 ... -> v5
        * ...     ...  ...  ...
        *
        * will yield the following two matrices:
        *
        * - leftGroup
        * p12  p13  p14 ... -> v1
        * p22  p23  p24 ... -> v2
        * p32  p33  p34 ... -> v3
        *
        * - rightGroup
        * pB   p42  p43  p44 ... -> v4
        * p51  p52  p53  p54 ... -> v5
        * ...  ...  ...  ...
        *
        *
        * The leftGroup's items are groupped by the required-value of the head-predicate.
        * Each group is the input for the thisTrie.children(required-value) sub Trie.
        *
        * The rightGroup is the input for the thisTrie.fallback sub Trie.
        *
        * Sub Tries are built using this very function recursively.
        *
        * @see ReorderCellsInEachRow to see the transformations that are applied to the rule-matrix
        *      in order to make the grouppings in the above-mentioned leftGroup more likely.
        */
      case (thisRow @ (node +: nodes, value)) +: rows =>
        val leftGroup = thisRow +: rows.takeWhile(_._1.headOption.exists(_.property == node.property))
        val rightGroup = rows.dropWhile(_._1.headOption.exists(_.property == node.property))

        val children =
          leftGroup
            .groupBy(_._1.head.value)
            .map { case (childKey, rows: Seq[R]) =>
              childKey -> buildTrie(
                rows.collect { case (_ :: childPath, childValue) => childPath -> childValue }) }



        Trie.WithChildren(node.property, children, buildTrie(rightGroup))

      /**
        * No rules left — it's a DeadEnd.
        */
      case Seq() =>
        Trie.DeadEnd
    }

  override def apply(matrix: M): IntoTrie.Trie[V] =
    buildTrie(matrix)
}
