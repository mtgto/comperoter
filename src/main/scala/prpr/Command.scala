package prpr

sealed trait PrprCommand

/**
 * スタックに値が不要なコマンド
 */
sealed trait ZeroStackCommand extends PrprCommand

/**
 * スタックに1つ以上値が必要なコマンド
 */
sealed trait OneStackCommand extends PrprCommand

/**
 * スタックに2つ以上値が必要なコマンド
 */
sealed trait TwoStackCommand extends PrprCommand

case class Push(num: Float) extends ZeroStackCommand
case class Dup() extends OneStackCommand
case class Swap() extends TwoStackCommand
case class Pop() extends OneStackCommand
case class Add() extends TwoStackCommand
case class Sub() extends TwoStackCommand
case class Mul() extends TwoStackCommand
case class Div() extends TwoStackCommand
case class Mod() extends TwoStackCommand
case class Store() extends TwoStackCommand
case class Load() extends OneStackCommand
case class Label(label: Float) extends ZeroStackCommand
case class Jump(label: Float) extends ZeroStackCommand
case class JZero(label: Float) extends OneStackCommand
case class JNeg(label: Float) extends OneStackCommand
case class End() extends ZeroStackCommand
case class PrintChar() extends OneStackCommand
case class PrintNum() extends OneStackCommand
case class ReadChar() extends ZeroStackCommand
case class ReadNum() extends ZeroStackCommand
