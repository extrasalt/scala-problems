package in.extrasalt.scalapblms

object TruthTable {
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

class Expression(expression: String) {
  def postfix(): String = {
    val list = expression.replaceAll("\\s", "").toList

    def popUntilOpening(stack: List[Char], infix: List[Char], postfix: List[Char]): List[Char] = {
      if (stack.head == '(') eval(stack.tail, infix, postfix)
      else popUntilOpening(stack.tail, infix, postfix :+ stack.head)
    }

    def eval(stack: List[Char], infix: List[Char], postfix: List[Char]): List[Char] = {
      if (infix.isEmpty) postfix ::: stack
      else
        infix.head match {
          case 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' =>
            eval(stack, infix.tail, postfix :+ infix.head)
          case '&' | '|' | '!' | '(' => eval(infix.head :: stack, infix.tail, postfix)
          case ')'                   => popUntilOpening(stack, infix.tail, postfix)
        }
    }

    eval(stack = List(), list, postfix = List()).mkString("")
  }

}

class Postfix(expression: String) {
  def isTautology: Boolean = {

    def evalExpWithRow(row: Map[String, Boolean]): Boolean = {
      val processedList = expression.toList.map(_.toString).map((x) => if (row.contains(x)) row(x).toString else x)

      def eval(stack: List[String], expression: List[String]): Boolean = {
        def handleOp(operator: String, stack: List[String], tail: List[String]): Boolean = {
          val a        = stack.tail.head.toBoolean
          val b        = stack.head.toBoolean
          val newStack = stack.tail.tail

          val opresult = operator match {
            case "&" => (a && b).toString
            case "|" => (a || b).toString
          }

          eval(newStack, opresult :: tail)
        }

        def handleNot(stack: List[String], tail: List[String]): Boolean = {
          val a        = stack.head.toBoolean
          val newStack = stack.tail
          val opresult = (!a).toString

          eval(newStack, opresult :: tail)
        }

        if (expression.isEmpty) return stack.head.toBoolean
        expression.head match {
          case "true" | "false" => eval(expression.head :: stack, expression.tail)
          case "&" | "|"        => handleOp(expression.head, stack, expression.tail)
          case "!"              => handleNot(stack, expression.tail)
        }
      }

      eval(List(), processedList)
    }

    val varSet = expression
      .replaceAll("[&|\\|\\(\\)\\!]", "")
      .map((x) => x.toString)
      .toSet

    val truthTable = TruthTable.generateTruthTable(varSet)

    def eval(tt: List[Map[String, Boolean]]): Boolean = {
      tt.forall((x) => evalExpWithRow(x))
    }

    eval(truthTable)

  }
}
