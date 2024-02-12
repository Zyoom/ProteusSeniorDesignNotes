import java.io.File
import scala.io.Source

object Tokenization {

  // Define a sealed trait Token which represents different types of tokens
  sealed trait Token

  // Define case classes for each type of token
  case class Identifier(name: String) extends Token
  case class Number(value: Int) extends Token
  case class ProteusCodeBlock(code: String) extends Token
  case object HashHash extends Token // Define a singleton object for '##'

  // Function to tokenize the input string
  def tokenize(input: String): List[Token] = {
    val proteusBlockStartEnd = "##" // Define the string representing the start and end of Proteus code blocks

    // Defined a tail-recursive helper function to tokenize the input string
    @scala.annotation.tailrec
    def tokenizeRec(input: List[Char], acc: List[Token], currentToken: StringBuilder, proteusBlockContent: StringBuilder, isProteusBlock: Boolean): List[Token] = {
      input match {

        /*case Nil =>
          if (currentToken.nonEmpty) {
            acc :+ (if (currentToken.forall(_.isDigit))
              Number(currentToken.toString.toInt) // If current token consists of digits, create a Number token
            else
              Identifier(currentToken.toString)) // If current token is not a number, create an Identifier token
          } else {
            acc
          }*/
        // If the input list is empty, return the accumulated tokens
        case Nil => // Got rid of Identifier output hoping we end with only complete tokens
          acc
        // If '##' is encountered and not inside a Proteus block, add a ProteusCodeBlock token to the accumulator
        case '#' :: '#' :: tail if !isProteusBlock =>
          tokenizeRec(tail,
            acc :+ ProteusCodeBlock(proteusBlockContent.toString),
            new StringBuilder,
            new StringBuilder,
            isProteusBlock = true)
        // If '##' is encountered and inside a Proteus block, add a ProteusCodeBlock token to the accumulator
        case '#' :: '#' :: tail if isProteusBlock =>
          tokenizeRec(tail,
            acc :+ ProteusCodeBlock(proteusBlockContent.toString),
            new StringBuilder,
            new StringBuilder,
            isProteusBlock = false)
        // If '#' is encountered:
        case '#' :: tail =>
          // If inside a Proteus block, add it to the Proteus block content, otherwise, add it to the current token
          if (isProteusBlock) {
            proteusBlockContent.append('#')
          } else {
            currentToken.append('#')
          }
          // Continue tokenizing the remaining input
          tokenizeRec(tail, acc, currentToken, proteusBlockContent, isProteusBlock)
        // If other characters are encountered:
        case c :: tail =>
          // If inside a Proteus block, append the character to the Proteus block content, otherwise, append it to the current token
          if (isProteusBlock) {
            proteusBlockContent.append(c)
          } else {
            currentToken.append(c)
          }
          // Continue tokenizing the remaining input
          tokenizeRec(tail, acc, currentToken, proteusBlockContent, isProteusBlock)
      }
    }

    // Start the tokenization process with an empty accumulator and StringBuilder for current token and Proteus block content
    tokenizeRec(input.toList, List.empty, new StringBuilder, new StringBuilder, isProteusBlock = false)
  }
}

object TokenizationExample {

  def main(args: Array[String]): Unit = {
    // Define the path to the file
    val filePath = "/Users/davidpedroza/Desktop/Proteus/src/main/scala/Tokens.txt"
    // Read the content of the file
    val fileContent = Source.fromFile(new File(filePath)).mkString
    // Tokenize the file content
    val tokens = Tokenization.tokenize(fileContent)
    // Print the tokens
    println(s"$tokens")
  }
}
