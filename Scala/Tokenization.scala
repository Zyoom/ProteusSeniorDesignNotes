import scala.io.Source
import java.io.File

object Tokenization {

  sealed trait Token

  // Define your token case classes or objects here
  case class Identifier(name: String) extends Token

  case class Number(value: Int) extends Token

  case object Plus extends Token

  case object Minus extends Token

  case object Multiply extends Token

  case object Divide extends Token

  case object OpenParenthesis extends Token

  case object CloseParenthesis extends Token

  case class FPPCodeBlock(code: String) extends Token

  case class ProteusCodeBlock(code: String) extends Token

  def tokenize(input: String): List[Token] = {
    var tokens: List[Token] = List.empty
    val currentToken: StringBuilder = new StringBuilder
    var isFPPBlock = false
    val fppBlockContent = new StringBuilder
    var isProteusBlock = false
    val proteusBlockContent = new StringBuilder

    def isOperator(c: Char): Boolean = Set('+', '-', '*', '/').contains(c)

    def isParenthesis(c: Char): Boolean = Set('(', ')').contains(c)

    for (char <- input) {
      if (char.isWhitespace) {
        // Ignore whitespace
      }
      else if (char == '[') {
        // Start of FPP code block
        isFPPBlock = true
        currentToken.append(char)
      }
      else if (char == '*' && isFPPBlock) {
        // End of FPP code block
        isFPPBlock = false
        currentToken.append(char)
        tokens = tokens :+ FPPCodeBlock(fppBlockContent.toString)
        fppBlockContent.clear()
      }
      else if (char == '(' && currentToken.endsWith("(*)")) {
        // StaRt of Proteus code block
        isProteusBlock = true
        currentToken.append(char)
      }
      else if (char == ')' && isProteusBlock) {
        // Ed of Proteus code block
        isProteusBlock = false
        currentToken.append(char)
        tokens = tokens :+ ProteusCodeBlock(proteusBlockContent.toString) // new instance made, & convert to string
        proteusBlockContent.clear()
      }
      else if (isFPPBlock) {
        // Inside FPP code block
        fppBlockContent.append(char)
      }
      else if (isProteusBlock) {
        // Iside Proteus code block
        proteusBlockContent.append(char)
      }
      else if (char.isDigit) {
        // Handle numbers
        currentToken.append(char)
      }
      else if (char.isLetter) {
        // Handle identifiers
        currentToken.append(char)
      }
      else if (isOperator(char)) {
        // Hadle operators
        tokens = tokens :+ (char match {
          case '+' => Plus
          case '-' => Minus
          case '*' => Multiply
          case '/' => Divide
        })
      }
      else {
        // Handle other characters if needed
      }
    }

    // Handle the last token, if any
    if (currentToken.nonEmpty) {
      tokens = tokens :+ (if (currentToken.forall(_.isDigit)) Number(currentToken.toString.toInt) else Identifier(currentToken.toString))
    }

    tokens
  }
}

object TokenizationExample {

  def main(args: Array[String]): Unit = {
    // Specify the path to your input file
    val filePath = "path/to/your/BLANK.txt"

    // Read the content of the file
    val fileContent = Source.fromFile(new File(filePath)).mkString

    // Tokenize the content
    val tokens = Tokenization.tokenize(fileContent)

    // Print the resulting tokens
    println(s"Tokens: $tokens")
  }
}
