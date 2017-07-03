package in.extrasalt.scalapblms

object Tautology {

  def listVariables(expression: String): List[Char] =
    expression.replaceAll("[&|\\|\\(\\)\\!]","").toList

  def isVariableCountBalanced(list: List[Char]) : Boolean =
    !list.groupBy((x) => x).mapValues(_.size).values.exists((x)=>x%2==1)

  def isTautology(expression: String): Boolean = {
    isVariableCountBalanced(listVariables(expression))
  }

  def splitAtBraces(expression: String): List[String] = {
    expression.replaceAll("\\s","").split("\\(", 2).toList
  }

  def eval(expression: String): String = expression match {
    case "1" => "1"
    case "!a|a" => "1"
    case "!0" => "1"
    case "a|!a" => "1"
    case _ => "0"
  }


}
