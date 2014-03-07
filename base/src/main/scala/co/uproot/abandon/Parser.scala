package co.uproot.abandon

import scala.language.postfixOps
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.json.Lexer
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.ImplicitConversions
import org.joda.time.LocalDate


class AbandonLexer extends StdLexical with ImplicitConversions {
  import scala.util.parsing.input.CharArrayReader.EofCh
  override def token: Parser[Token] =
    //( '\"' ~ rep(charSeq | letter) ~ '\"' ^^ lift(StringLit)
    (string ^^ StringLit
      | identChar ~ rep(identChar | digit) ^^ { case first ~ rest => processIdent(first :: rest mkString "") }
      | number ~ letter ^^ { case n ~ l => ErrorToken("Invalid number format : " + n + l) }
      | '-' ~> number ^^ { case num => NumericLit("-" + num) }
      | number ^^ NumericLit
      | eol ^^^ EOL
      | comment ^^ { case commentContents => CommentToken(commentContents.toString)}
      | EofCh ^^^ EOF
      | delim
      | '\"' ~> failure("Unterminated string")
      | rep(letter) ^^ checkKeyword
      | failure("Illegal character")
    )

  case object EOL extends Token {
    def chars = "<eol>"
  }
  case class CommentToken(commentContents : String) extends Token {
    def chars = commentContents
  }

  def checkKeyword(xs: List[Any]) = {
    val strRep = xs mkString ""
    if (reserved contains strRep) Keyword(strRep) else ErrorToken("Not a keyword: " + strRep)
  }

  def eol = elem("eol", _ == '\n')
  override def comment = ';' ~> rep(chrExcept(EofCh, '\n')) ^^ {case chars => chars.mkString}

  /**
   * A string is a collection of zero or more Unicode characters, wrapped in
   *  double quotes, using backslash escapes (cf. http://www.json.org/).
   */
  def string = '\"' ~> rep(charSeq | chrExcept('\"', '\n', EofCh)) <~ '\"' ^^ { _ mkString "" }

  override def whitespaceChar = elem("space char", ch => ch <= ' ' && ch != '\n' && ch != EofCh)
  override def whitespace = rep(whitespaceChar)

  def number = intPart ~ opt(fracPart) ~ opt(expPart) ^^ {
    case i ~ f ~ e =>
      i + optString(".", f) + optString("", e)
  }
  def intPart = (zero*) ~> intList
  def intList = (nonzero ~ ((comma ~> rep1sep(digit, comma?)) | repsep(digit, comma?)))  ^^ { case x ~ y => (x :: y) mkString "" }
  def fracPart = '.' ~> rep(digit) ^^ { _ mkString "" }
  def expPart = exponent ~ opt(sign) ~ rep1(digit) ^^ {
    case e ~ s ~ d =>
      e + optString("", s) + d.mkString("")
  }

  private def optString[A](pre: String, a: Option[A]) = a match {
    case Some(x) => pre + x.toString
    case None    => ""
  }

  def comma: Parser[String] = ',' ^^^ ","
  def zero: Parser[String] = '0' ^^^ "0"
  def nonzero = elem("nonzero digit", d => d.isDigit && d != '0')
  def exponent = elem("exponent character", d => d == 'e' || d == 'E')
  def sign = elem("sign character", d => d == '-' || d == '+')

  def charSeq: Parser[String] =
    ('\\' ~ '\"' ^^^ "\""
      | '\\' ~ '\\' ^^^ "\\"
      | '\\' ~ '/' ^^^ "/"
      | '\\' ~ 'b' ^^^ "\b"
      | '\\' ~ 'f' ^^^ "\f"
      | '\\' ~ 'n' ^^^ "\n"
      | '\\' ~ 'r' ^^^ "\r"
      | '\\' ~ 't' ^^^ "\t"
      | '\\' ~> 'u' ~> unicodeBlock)

  val hexDigits = Set[Char]() ++ "0123456789abcdefABCDEF".toArray
  def hexDigit = elem("hex digit", hexDigits.contains(_))

  private def unicodeBlock = hexDigit ~ hexDigit ~ hexDigit ~ hexDigit ^^ {
    case a ~ b ~ c ~ d =>
      new String(Array(Integer.parseInt(List(a, b, c, d) mkString "", 16)), 0, 1)
  }

}

object AbandonParser extends StandardTokenParsers with PackratParsers {
  override val lexical = new AbandonLexer
  private val defKeyword = "def"
  private val accountKeyword = "account"
  private val includeKeyword = "include"
  private val trueKeyword = "true"
  private val falseKeyword = "false"
  private val payeeKeyword = "payee"
  private val tagKeyword = "tag"
  lexical.reserved += defKeyword
  lexical.reserved += accountKeyword
  lexical.reserved += includeKeyword
  lexical.reserved += trueKeyword
  lexical.reserved += falseKeyword
  lexical.reserved += payeeKeyword
  lexical.reserved += tagKeyword
  lexical.delimiters ++= List("+", "-", "*", "/", "=", "(", ")", ":", ",", "&", ".")

  private lazy val integer = numericLit ^^ { case x => x.toInt }
  private lazy val number:PackratParser[BigDecimal] = accept("number", { case lexical.NumericLit(n) => BigDecimal(n) })
  private lazy val eol = accept("<eol>", { case lexical.EOL => })
  private lazy val comment = accept("<comment>", { case lexical.CommentToken(c) => c})
  private lazy val anyEol = ((comment?) ~ eol)
  private lazy val allButEOL:PackratParser[String] = accept("any", {
    case t:lexical.Token if !t.isInstanceOf[lexical.EOL.type] && !t.isInstanceOf[lexical.ErrorToken] => t.chars
  })
  private lazy val allUntilEOL = (allButEOL+) ^^ {case tokens => tokens.mkString("")}
  private lazy val stringOrAllUntilEOL = stringLit | allUntilEOL

  private lazy val fragSeparators = anyEol*
  private def line[T](p: Parser[T]): Parser[T] = p <~ (((comment?)~eol)*)

  // End of line commment
  private def eolComment  = (comment?)<~eol

  lazy val abandon:Parser[Seq[ASTEntry]] = phrase((fragSeparators ~> repsep(fragment, fragSeparators)) <~ fragSeparators)

  private lazy val fragment:Parser[ASTEntry] = (txFrag | defFrag | accountDefFrag | includeFrag | payeeDefFrag | tagDefFrag)
  private lazy val includeFrag = (includeKeyword ~> fileName) ^^ {case name => IncludeDirective(name)}
  private lazy val payeeDefFrag = (payeeKeyword ~> stringOrAllUntilEOL) ^^ {case payee => PayeeDef(payee.mkString(""))}
  private lazy val tagDefFrag = (tagKeyword ~> stringOrAllUntilEOL) ^^ {case tag => TagDef(tag.mkString(""))}
   
  private lazy val fileName = stringOrAllUntilEOL

  private lazy val accountDefFrag = ((accountKeyword ~> accountName) <~ anyEol) ~ accountDefDetails ^^ {
    case name ~ details => AccountDeclaration(name, details)
   }
  private lazy val accountDefDetails = keyValuePairs
  private lazy val keyValuePairSingleton = (ident) ^^ {case k => (k, BooleanLiteralExpr(true))}
  private lazy val keyValuePairBoolean = ((ident <~ ":") ~ booleanExpression) ^^ {case k ~ v => (k,v)}
  private lazy val keyValuePair = keyValuePairSingleton | keyValuePairBoolean
  private lazy val keyValuePairs = ((keyValuePair <~ anyEol)*) ^^ {case s => s.toMap}

  private lazy val defFrag = (((defKeyword ~> ident) ~ (arguments?)) <~ "=") ~ expression ^^ {
    case name ~ arg ~ expr => Definition(name, arg.getOrElse(Nil), expr)
  }
  private lazy val arguments = ("(" ~> rep1sep(ident, ",")) <~ ")"
  private lazy val expression:PackratParser[Expr[_]] = (numericExpr | booleanExpression )

  private def mkFunctionExpr[T](exprParser:Parser[Expr[T]]) = {
    val zeroArgFunctionExpr = (ident ^^ {case x => IdentifierExpr[T](x)})
    val multiArgFunctionExpr = (((ident <~ "(") ~ rep1sep(exprParser, ",")) <~ ")") ^^ {case x ~ es => FunctionExpr[T](x, es)}
    multiArgFunctionExpr ||| zeroArgFunctionExpr
  }

  private lazy val numericFunctionExpr = mkFunctionExpr(numericExpr)

  import ASTHelper.NumericExpr

  private lazy val numericExpr:PackratParser[NumericExpr] = (addExpr | subExpr | mulExpr | divExpr | numericLiteralExpr | numericFunctionExpr | unaryNegExpr | parenthesizedExpr)
  private lazy val numericLiteralExpr:PackratParser[NumericExpr] = (number ^^ {case n => NumericLiteralExpr(n)})
  private lazy val addExpr:PackratParser[AddExpr] = ((numericExpr <~ "+") ~ numericExpr) ^^ { case a ~ b => AddExpr(a, b) }
  private lazy val subExpr:PackratParser[SubExpr] = ((numericExpr <~ "-") ~ numericExpr) ^^ { case a ~ b => SubExpr(a, b) }
  private lazy val mulExpr:PackratParser[MulExpr] = ((numericExpr <~ "*") ~ numericExpr) ^^ { case a ~ b => MulExpr(a, b) }
  private lazy val divExpr:PackratParser[DivExpr] = ((numericExpr <~ "/") ~ numericExpr) ^^ { case a ~ b => DivExpr(a, b) }
  private lazy val unaryNegExpr:PackratParser[UnaryNegExpr] = ("-" ~> numericExpr) ^^ { case expr => UnaryNegExpr(expr) }
  private lazy val parenthesizedExpr:PackratParser[NumericExpr] = (("(" ~> numericExpr) <~ ")") ^^ { case expr => expr }

  private lazy val booleanExpression:PackratParser[BooleanExpr] = (trueKeyword ^^^ BooleanLiteralExpr(true)) | (falseKeyword ^^^ BooleanLiteralExpr(false))

  private lazy val txFrag = ((dateFrag ~ (annotation?) ~ (payee?)) <~ eol) ~ (eolComment*) ~ (txDetails+) ^^ {
    case date ~ annotationOpt ~ optPayee ~ optComment ~ transactions =>
      val annotationStrOpt = annotationOpt.map(_.mkString(""))
      Transaction(date, transactions, annotationStrOpt, optPayee, optComment.flatten)
  }
  private lazy val annotation = (("(" ~> (ident|numericLit)+) <~ ")")
  private lazy val payee = ((allButEOL)+) ^^ {case x => x.mkString(" ")}
  private lazy val txDetails:PackratParser[SingleTransaction] = (accountName ~ opt(numericExpr) ~ eolComment) ^^ {
    case name ~ amount ~ commentOpt => SingleTransaction(name, amount, commentOpt)
  }
  private lazy val accountName = rep1sep(ident, ":") ^^ { case path => AccountName(path) }

  private lazy val dateFrag = ((((integer <~ "/") ~ (integer | ident)) <~ "/") ~ integer) ^? ({
    case y ~ (m: Int) ~ d if (isValidDate(y, m, d)) =>
      Date(y, m, d)
    case y ~ (m: String) ~ d if (Helper.getMonthNumber(m).isDefined && isValidDate(y, Helper.getMonthNumber(m).get, d)) =>
      Date(y, Helper.getMonthNumber(m).get, d)
  }, { case y ~ m ~ d => List(y, m, d).mkString("/") + " is not a valid calendar date" })

  private def isValidDate(y: Int, m: Int, d: Int) = {
    try {
      new LocalDate(y, m, d)
      true
    } catch {
      case _:org.joda.time.IllegalFieldValueException => false
    }
  }
}