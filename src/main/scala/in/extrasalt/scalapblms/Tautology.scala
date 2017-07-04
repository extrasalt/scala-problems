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

  def handleOp(operator: Char,
               stack: List[Char],
               tail: List[Char],
               eval: (List[Char], List[Char]) => List[Char]): List[Char] = {
    val a        = stack.tail.head
    val b        = stack.head
    val newStack = stack.tail.tail

    val opresult = operator match {
      case '&' => '1'
      case '|' => '1'
    }
    //TODO: Evaluate a op b

    eval(newStack, opresult :: tail)
  }

  def evaluatePostfix(expression: String): String = {
    val list = expression.toList

    def eval(stack: List[Char], postfix: List[Char]): List[Char] = {
      postfix.head match {
        case 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' =>
          eval(postfix.head :: stack, postfix.tail)
        case '&' => handleOp('&', stack, postfix.tail, eval)
        case '|' => handleOp('|', stack, postfix.tail, eval)

      }
    }

    eval(stack = List(), list).mkString("")

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

}
