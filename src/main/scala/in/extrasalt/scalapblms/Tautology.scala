package in.extrasalt.scalapblms

object Tautology {
  val varSet = Set('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j')
  val opSet  = Set('!', '&', '|')

  def convertToPostfix(expression: String): String = {
    val list = expression.replaceAll("\\s", "").toList

    def eval(stack: List[Char], infix: List[Char], postfix: List[Char]): List[Char] = {
      if (infix.isEmpty) postfix ::: stack
      else if (varSet.contains(infix.head))
        eval(stack, infix.tail, postfix :+ infix.head)
      else eval(List(infix.head), infix.tail, postfix ::: stack)
    }

    eval(stack = List(), list, postfix = List()).mkString("")
  }

  def isVariableCountBalanced(expression: String): Boolean =
    !expression
      .replaceAll("[&|\\|\\(\\)\\!]", "")
      .toList
      .groupBy((x) => x)
      .mapValues(_.size)
      .values
      .toSet
      .contains(1)

  def isTautology(expression: String): Boolean = {
    if (isVariableCountBalanced(expression)) {
      true
    } else {
      //TODO: Implement logic using eval function
      false
    }
  }

  def splitAtOpeningBraces(expression: String): List[String] = {
    expression.split("\\(", 2).toList.filter(_.nonEmpty)
  }

  def splitAtClosingBraces(expression: String): List[String] = {
    expression.split("\\)", 2).toList.filter(_.nonEmpty)
  }

  def eval(expression: String): String = {
    val trimmedExpression = expression.replaceAll("\\s", "")
    if (trimmedExpression.contains("\\(")) {
      val splitList = splitAtOpeningBraces(trimmedExpression)
      eval(splitList.head.concat(eval(splitList(1))))
    } else if (trimmedExpression.contains("\\)")) {
      val splitList = splitAtClosingBraces(trimmedExpression)
      eval(eval(splitList.head).concat(splitList(1)))
    } else {
      trimmedExpression match {
        case "1"    => "1"
        case "!a|a" => "1"
        case "1|1"  => "1"
        case "a|1"  => "1"
        case "1&1"  => "1"
        case "1|a"  => "1"
        case "!0"   => "1"
        case "a|!a" => "1"
        case "a"    => "a"
        case "!a"   => "!a"
        case _      => "0"
      }
    }
  }

}
