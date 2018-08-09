package com.github.rgafiyatullin.la_rete

object Filter {
  trait HasAsOp[-In, +Out] {
    def as[OutNext]: Filter[In, OutNext]
  }

  def property[Obj, Prop]
    (p: Property[Obj, Prop],
     v: Prop)
  : Filter[Obj, Obj] with HasAsOp[Obj, Obj] =
    PropertyIs(p, v)


  private final case class Or[-In, +Out]
    (left: Filter[In, Out],
     right: Filter[In, Out])
    extends Filter[In, Out]

  private final case class And[-In, Med, +Out]
    (left: Filter[In, Med],
     right: Filter[Med, Out])
    extends Filter[In, Out]

  private final case class PropertyIs[ObjIn, Value, ObjOut]
    (property: Property[ObjIn, Value],
     value: Value)
    extends Filter[ObjIn, ObjOut]
      with HasAsOp[ObjIn, ObjOut]
  {
    def as[ObjOutNext]: Filter[ObjIn, ObjOutNext] =
      PropertyIs[ObjIn, Value, ObjOutNext](property, value)
  }
}

trait Filter[-In, +Out] {
  final def or[In1 <: In, Out1 >: Out]
    (other: Filter[In1, Out1])
  : Filter[In1, Out1] =
    Filter.Or(this, other)

  final def and[Med >: Out, OutNext]
    (other: Filter[Med, OutNext])
  : Filter[In, OutNext] =
    Filter.And(this, other)
}
