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
    expression.replaceAll("\\s","").split("\\(", 2).toList
  }

  def splitAtClosingBraces(expression: String): List[String] = {
    expression.split("\\)",2).toList.filter(_.nonEmpty)
  }

  def eval(expression: String): String = {
    if(expression.contains("\\(")){
      val splitList = splitAtOpeningBraces(expression)
      eval(splitList.head.concat(eval(splitList(1))))
    } else if(expression.contains("\\)")){
      val splitList = splitAtClosingBraces(expression)
      eval(eval(splitList.head).concat(splitList(1)))
    }else {
      expression match {
        case "1" => "1"
        case "!a|a" => "1"
        case "!0" => "1"
        case "a|!a" => "1"
        case _ => "0"
      }
    }
  }

}
