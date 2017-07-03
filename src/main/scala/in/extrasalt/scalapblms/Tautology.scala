package in.extrasalt.scalapblms

object Tautology {

  def listVariables(expression: String): List[Char] =
    expression.replaceAll("[&|\\|\\(\\)\\!]","").toList

  def isVariableCountBalanced(list: List[Char]) : Boolean =
    !list.groupBy((x) => x).mapValues(_.size).values.exists((x)=>x%2==1)

  def isTautology(expression: String): Boolean = {
    if(isVariableCountBalanced(listVariables(expression))){
     true
    } else false
  }

  def splitAtOpeningBraces(expression: String): List[String] = {
    expression.split("\\(", 2).toList.filter(_.nonEmpty)
  }

  def splitAtClosingBraces(expression: String): List[String] = {
    expression.split("\\)",2).toList.filter(_.nonEmpty)
  }

  def eval(expression: String): String = {
    val trimmedExpression = expression.replaceAll("\\s", "")
    if(trimmedExpression.contains("\\(")){
      val splitList = splitAtOpeningBraces(trimmedExpression)
      eval(splitList.head.concat(eval(splitList(1))))
    } else if(trimmedExpression.contains("\\)")){
      val splitList = splitAtClosingBraces(trimmedExpression)
      eval(eval(splitList.head).concat(splitList(1)))
    }else {
      trimmedExpression match {
        case "1" => "1"
        case "!a|a" => "1"
        case "1|1" => "1"
        case "a|1" => "1"
        case "1&1" => "1"
        case "1|a" => "1"
        case "!0" => "1"
        case "a|!a" => "1"
        case "a" => "a"
        case "!a" => "!a"
        case _ => "0"
      }
    }
  }

}
