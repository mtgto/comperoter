package prpr

sealed trait PrprCommand

case class Push(num: Float) extends PrprCommand
case class Dup() extends PrprCommand
case class Swap() extends PrprCommand
case class Pop() extends PrprCommand
case class Add() extends PrprCommand
case class Sub() extends PrprCommand
case class Mul() extends PrprCommand
case class Div() extends PrprCommand
case class Mod() extends PrprCommand
case class Store() extends PrprCommand
case class Load() extends PrprCommand
case class Label(label: Float) extends PrprCommand
case class Jump(label: Float) extends PrprCommand
case class JZero(label: Float) extends PrprCommand
case class JNeg(label: Float) extends PrprCommand
case class End() extends PrprCommand
case class PrintChar() extends PrprCommand
case class PrintNum() extends PrprCommand
case class ReadChar() extends PrprCommand
case class ReadNum() extends PrprCommand
