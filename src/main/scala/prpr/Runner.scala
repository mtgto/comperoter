package prpr

import scala.annotation.tailrec

class Runner(prpr:String) {
  def generateLabelMap(commands:List[PrprCommand], map:Map[Float, List[PrprCommand]]):Map[Float, List[PrprCommand]] = {
    commands match {
      case hd::tl =>
	hd match {
	  case Label(label) => generateLabelMap(tl, map.updated(label, tl))
	  case _ => generateLabelMap(tl, map)
	}
      case _ => map
    }
  }

  def execute(source:String) = {
    val parser = new Parser(prpr)
    val parseResult = parser.parseAll(parser.program, source)
    parseResult match {
      case parser.Success(commands, _) =>
	val labelMap = generateLabelMap(commands, Map())
	executeCommands(commands, List(), Map(), labelMap)
      case _ =>
	throw new RuntimeException("failed to parse the program.")
    }
  }

  @tailrec
  private def executeCommands(commands:List[PrprCommand], stack:List[Float], heap:Map[Int, Float], labelMap:Map[Float, List[PrprCommand]]): Unit = {
    commands match {
      case hd::tl => {
	hd match {
	  case command:ZeroStackCommand =>
	    command match {
	      case Push(num) =>
		executeCommands(tl, num::stack, heap, labelMap)
	      case Label(label) =>
		executeCommands(tl, stack, heap, labelMap)
	      case Jump(label) =>
		labelMap.get(label) match {
		  case Some(list) =>
		    executeCommands(list, stack, heap, labelMap)
		  case _ =>
		    throw new RuntimeException("no label is defined")
		}
	      case End() =>
		println("finished")
	      case ReadChar() =>
		print("Please input character: ")
		val char = readLine().head.toFloat
		executeCommands(tl, char::stack, heap, labelMap)
	      case ReadNum() =>
		print("Please input number: ")
		val num = readLine().toInt
		executeCommands(tl, num::stack, heap, labelMap)
	    }
	  case command:OneStackCommand =>
	    stack match {
	      case fst::rest =>
		command match {
		  case Dup() =>
		    executeCommands(tl, fst::fst::rest, heap, labelMap)
		  case Pop() =>
		    executeCommands(tl, rest, heap, labelMap)
		  case Load() =>
		    heap.get(fst.toInt) match {
		      case Some(value) => executeCommands(tl, value::rest, heap, labelMap)
		      case _ => throw new RuntimeException("invalid indexes: " + fst.toInt)
		    }
		  case JZero(label) =>
		    labelMap.get(label) match {
		      case Some(list) =>
			if (fst == 0)
			  executeCommands(list, rest, heap, labelMap)
			else
			  executeCommands(tl, rest, heap, labelMap)
		      case _ =>
			throw new RuntimeException("no label is defined")
		    }
		  case JNeg(label) =>
		    labelMap.get(label) match {
		      case Some(list) =>
			if (fst < 0)
			  executeCommands(list, rest, heap, labelMap)
			else
			  executeCommands(tl, rest, heap, labelMap)
		      case _ =>
			throw new RuntimeException("no label is defined")
		    }
		  case PrintChar() =>
		    print(fst.toChar); executeCommands(tl, stack, heap, labelMap)
		  case PrintNum() =>
		    print(fst.toInt); executeCommands(tl, stack, heap, labelMap)
		}
	      case _ =>
		throw new RuntimeException("stack is empty")
	    }
	  case command:TwoStackCommand =>
	    stack match {
	      case fst::snd::rest => {
		command match {
		  case Swap() =>
		    executeCommands(tl, snd::fst::rest, heap, labelMap)
		  case Add() =>
		    executeCommands(tl, (snd+fst)::rest, heap, labelMap)
		  case Sub() =>
		    executeCommands(tl, (snd-fst)::rest, heap, labelMap)
		  case Mul() =>
		    executeCommands(tl, (snd*fst)::rest, heap, labelMap)
		  case Div() =>
		    if (snd == 0)
		      throw new RuntimeException("divide zero")
		    else
		      executeCommands(tl, (snd/fst)::rest, heap, labelMap)
		  case Mod() =>
		    if (snd == 0)
		      throw new RuntimeException("divide zero")
		    else
		      executeCommands(tl, (snd%fst)::rest, heap, labelMap)
		  case Store() =>
		    executeCommands(tl, rest, heap.updated(fst.toInt, snd), labelMap)
		}
	      }
	      case _ =>
		throw new RuntimeException("stack must have at leaset two values")
	    }
	}
      }
      case _ =>
	println("finished")
    }
  }
}
