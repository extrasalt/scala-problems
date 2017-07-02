package in.extrasalt.scalapblms

object Tautology {

  //The only possible tautologies are a & 1, !a | a, !(a & !a)
  //where a is any variable in a set of 10 variables.

  def listVariables(expression: String): List[Char] =
    expression.replaceAll("[&|\\|\\(\\)\\!]","").toList

  def isVariableCountBalanced(list: List[Char]) : Boolean =
    !list.groupBy((x) => x).mapValues(_.size).values.exists((x)=>x%2==1)
}
