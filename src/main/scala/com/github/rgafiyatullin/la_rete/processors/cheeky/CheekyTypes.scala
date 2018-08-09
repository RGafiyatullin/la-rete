package com.github.rgafiyatullin.la_rete.processors.cheeky

import com.github.rgafiyatullin.la_rete.Node

class CheekyTypes[V] {
  type Row = (Seq[Node], V)
  type Matrix = Seq[Row]
}
