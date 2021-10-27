package edu.KUP


case class Integer(v: Nat, sign: Sign) extends Nat with Sign {
  override def isZero: Boolean = v.isZero

  override def predecessor: Nat = {
    if (isZero) new Integer(v.successor, Negative)
    else if (isPositive) new Integer(v.predecessor, sign)
    else new Integer(v.predecessor, Negative)
  }

  override def successor: Nat = {
    if (isZero) new Integer(v.successor, Positive)
    else if (isPositive) new Integer(v.successor, sign)
    else new Integer(v.predecessor, Negative)
  }

  override def +(that: Nat): Nat = {
    if (isZero) that
    else if (sign.isPositive) this.predecessor + that.successor
    else this.successor + that.predecessor
  }

  override def -(that: Nat): Nat = {
    if (that.isZero) this
    else that match {
      case Integer(v, s) => this + new Integer(v, s.negate)
    }
  }


  def isPositive: Boolean = true

  def negate: Integer = new Integer(v, sign.negate)
}
