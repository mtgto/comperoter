package prpr

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

      case _ =>
	throw new RuntimeException("failed to parse the program.")
    }
  }
}
