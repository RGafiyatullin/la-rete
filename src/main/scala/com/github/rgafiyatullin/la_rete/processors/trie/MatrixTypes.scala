package com.github.rgafiyatullin.la_rete.processors.trie

import com.github.rgafiyatullin.la_rete.Node

class MatrixTypes[V] {
  type Row = (Seq[Node], V)
  type Matrix = Seq[Row]
}
