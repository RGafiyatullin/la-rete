package com.github.rgafiyatullin.la_rete

object Property {
  trait Untyped {
    def isDefinedAt(obj: Any): Boolean
    def extract(obj: Any): Option[Any]
  }

  trait Of[-Obj] extends Untyped {
    def apply(obj: Obj): Any

    final override def extract(obj: Any): Option[Any] =
      if (isDefinedAt(obj))
        Some(apply(obj.asInstanceOf[Obj]))
      else
        None
  }
}

trait Property[-Obj, +Prop] extends Property.Of[Obj] {
  override def apply(obj: Obj): Prop
}
