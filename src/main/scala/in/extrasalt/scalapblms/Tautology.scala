package in.extrasalt.scalapblms

import java.util.NoSuchElementException

object Tautology {

  def popUntilOpening(stack: List[Char],
                      infix: List[Char],
                      postfix: List[Char],
                      eval: (List[Char], List[Char], List[Char]) => List[Char]): List[Char] = {
    if (stack.head == '(') {
      eval(stack.tail, infix, postfix)
    } else {
      popUntilOpening(stack.tail, infix, postfix :+ stack.head, eval)
    }
  }

  def convertToPostfix(expression: String): String = {
    val list = expression.replaceAll("\\s", "").toList

    def eval(stack: List[Char], infix: List[Char], postfix: List[Char]): List[Char] = {
      if (infix.isEmpty) {
        postfix ::: stack
      } else {
        infix.head match {
          case 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' =>
            eval(stack, infix.tail, postfix :+ infix.head)
          case '&' | '|' | '!' | '(' => eval(infix.head :: stack, infix.tail, postfix)
          case ')'                   => popUntilOpening(stack, infix.tail, postfix, eval)
        }
      }
    }

    eval(stack = List(), list, postfix = List()).mkString("")
  }

  def handleOp(operator: String,
               stack: List[String],
               tail: List[String],
               eval: (List[String], List[String]) => List[String]): List[String] = {
    val a        = stack.tail.head.toBoolean
    val b        = stack.head.toBoolean
    val newStack = stack.tail.tail

    val opresult = operator match {
      case "&" => (a && b).toString
      case "|" => (a || b).toString
    }

    eval(newStack, opresult :: tail)
  }

  def handleNot(stack: List[String], tail: List[String], eval: (List[String], List[String]) => List[String]): List[String] = {
    val a = stack.head.toBoolean
    val newStack = stack.tail

    val opresult = (!a).toString

    eval(newStack, opresult::tail)
  }

  def evalExpression(expression: String, row: Map[String, Boolean]): Boolean = {
    val processedList = expression.toList.map(_.toString).map((x)=> if(row.contains(x)) row(x).toString else x)

    def eval(stack : List[String], expression: List[String]): List[String] = {
      if(expression.size == 1) return expression

      expression.head match {
        case "true" | "false" => eval(expression.head :: stack, expression.tail)
        case "&" | "|" => handleOp(expression.head, stack,expression.tail, eval)
        case "!" => handleNot(stack, expression.tail, eval)
      }
    }
    if(eval(stack=List(), processedList) == List("true")) true else false
  }

  def evaluatePostfix(expression: String): Boolean = {
    val varSet = expression
      .replaceAll("[&|\\|\\(\\)\\!]", "")
      .map((x) => x.toString)
      .toSet

    val truthTable = TruthTable.generateTruthTable(varSet)

    def eval(tt: List[Map[String, Boolean]]): Boolean = {
      tt.forall((x) => evalExpression(expression,x))
    }

    eval(truthTable)

  }

}

object TruthTable {
  def getVariables(expression: String): Set[String] = {
    expression
      .replaceAll("[&|\\|\\(\\)\\!]", "")
      .map((x) => x.toString)
      .toSet
  }

  def generateTruthTable(variables: Set[String]): List[Map[String, Boolean]] = {
    if (variables.size == 1) {
      List(Map(variables.head -> true), Map(variables.head -> false))
    } else {
      generateTruthTable(Set(variables.head)).flatMap { row =>
        generateTruthTable(variables.tail).map(row ++ _)
      }
    }
  }
}
