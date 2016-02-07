package co.uproot.abandon

import scala.language.postfixOps
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.json.Lexer
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.ImplicitConversions

class AbandonLexer extends StdLexical with ImplicitConversions {
  import scala.util.parsing.input.CharArrayReader.EofCh
  override def token: Parser[Token] =
    //( '\"' ~ rep(charSeq | letter) ~ '\"' ^^ lift(StringLit)
    (string ^^ StringLit
      | identChar ~ rep(identChar | digit) ^^ { case first ~ rest => processIdent(first :: rest mkString "") }
      | number ~ letter ^^ { case n ~ l => ErrorToken("Invalid number format : " + n + l) }
      | number ^^ NumericLit
      | eol ^^^ EOL
      | comment ^^ { case commentContents => CommentToken(commentContents.toString) }
      | EofCh ^^^ EOF
      | delim
      | '\"' ~> failure("Unterminated string")
      | rep(letter) ^^ checkKeyword
      | failure("Illegal character"))

  case object EOL extends Token {
    def chars = "<eol>"
  }
  case class CommentToken(commentContents: String) extends Token {
    def chars = commentContents
  }

  def checkKeyword(xs: List[Any]) = {
    val strRep = xs mkString ""
    if (reserved contains strRep) Keyword(strRep) else ErrorToken("Not a keyword: " + strRep)
  }

  def eol = elem("eol", _ == '\n')
  override def comment = ';' ~> rep(chrExcept(EofCh, '\n')) ^^ { case chars => chars.mkString }

  /** A string is a collection of zero or more Unicode characters, wrapped in
    * double quotes, using backslash escapes (cf. http://www.json.org/).
    */
  def string = '\"' ~> rep(charSeq | chrExcept('\"', '\n', EofCh)) <~ '\"' ^^ { _ mkString "" }

  override def whitespaceChar = elem("space char", ch => ch <= ' ' && ch != '\n' && ch != EofCh)
  override def whitespace = rep(whitespaceChar)

  def number = intPart ~ opt(fracPart) ~ opt(expPart) ^^ {
    case i ~ f ~ e =>
      i + optString(".", f) + optString("", e)
  }

  /** backtracking version of prefix* ~> suffix */
  def lazyPrefix[T](prefix: Parser[T], suffix: Parser[T]): Parser[T] = {
    (prefix ~> lazyPrefix(prefix, suffix)) | (prefix ~> suffix) | suffix
  }

  def intPart = lazyPrefix(zero, intList)
  def intList = (digit ~ ((comma ~> rep1sep(digit, comma?)) | repsep(digit, comma?))) ^^ { case x ~ y => (x :: y) mkString "" }
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

  val nameSeparator = ":"

  private lazy val integer = numericLit ^^ { case x => x.toInt }
  private lazy val number: PackratParser[BigDecimal] = accept("number", { case lexical.NumericLit(n) => BigDecimal(n) })
  private lazy val eol = accept("<eol>", { case lexical.EOL => })
  private lazy val comment = accept("<comment>", { case lexical.CommentToken(c) => c })
  private lazy val anyEol = ((comment?) ~ eol)
  private lazy val allButEOL: PackratParser[String] = accept("any", {
    case t: lexical.Token if !t.isInstanceOf[lexical.EOL.type] && !t.isInstanceOf[lexical.ErrorToken] => t.chars
  })
  private lazy val allUntilEOL = (allButEOL+) ^^ { case tokens => tokens.mkString("") }
  private lazy val stringOrAllUntilEOL = stringLit | allUntilEOL

  private lazy val fragSeparators = anyEol*
  private def line[T](p: Parser[T]): Parser[T] = p <~ (((comment?) ~ eol)*)

  // End of line commment
  private def eolComment = (comment?) <~ eol

  lazy val abandon: Parser[Seq[ASTEntry]] = phrase((fragSeparators ~> repsep(fragment, fragSeparators)) <~ fragSeparators)
  lazy val accountName: Parser[AccountName] = rep1sep(ident, nameSeparator) ^^ { case path => AccountName(path) }

  private lazy val fragment: Parser[ASTEntry] = (txFrag | defFrag | accountDefFrag | includeFrag | payeeDefFrag | tagDefFrag)
  private lazy val includeFrag = (includeKeyword ~> fileName) ^^ { case name => IncludeDirective(name) }
  private lazy val payeeDefFrag = (payeeKeyword ~> stringOrAllUntilEOL) ^^ { case payee => PayeeDef(payee.mkString("")) }
  private lazy val tagDefFrag = (tagKeyword ~> stringOrAllUntilEOL) ^^ { case tag => TagDef(tag.mkString("")) }

  private lazy val fileName = stringOrAllUntilEOL

  private lazy val accountDefFrag = ((accountKeyword ~> accountName) <~ anyEol) ~ accountDefDetails ^^ {
    case name ~ details => AccountDeclaration(name, details)
  }
  private lazy val accountDefDetails = keyValuePairs
  private lazy val keyValuePairSingleton = (ident) ^^ { case k => (k, BooleanLiteralExpr(true)) }
  private lazy val keyValuePairBoolean = ((ident <~ ":") ~ booleanExpression) ^^ { case k ~ v => (k, v) }
  private lazy val keyValuePair = keyValuePairSingleton | keyValuePairBoolean
  private lazy val keyValuePairs = ((keyValuePair <~ anyEol)*) ^^ { case s => s.toMap }

  private lazy val defFrag = (((defKeyword ~> ident) ~ (arguments?)) <~ "=") ~ expression ^^ {
    case name ~ arg ~ expr => Definition(name, arg.getOrElse(Nil), expr)
  }
  private lazy val arguments = ("(" ~> rep1sep(ident, ",")) <~ ")"
  private lazy val expression: PackratParser[Expr[_]] = (numericExpr | booleanExpression)

  private def mkFunctionExpr[T](exprParser: Parser[Expr[T]]) = {
    val zeroArgFunctionExpr = (ident ^^ { case x => IdentifierExpr[T](x) })
    val multiArgFunctionExpr = (((ident <~ "(") ~ rep1sep(exprParser, ",")) <~ ")") ^^ { case x ~ es => FunctionExpr[T](x, es) }
    multiArgFunctionExpr ||| zeroArgFunctionExpr
  }

  private lazy val numericFunctionExpr = mkFunctionExpr(numericExpr)

  import ASTHelper.NumericExpr

  lazy val numericParser: Parser[NumericExpr] = phrase(numericExpr)

  private lazy val numericLiteralExpr: PackratParser[NumericExpr] = (number ^^ { case n => NumericLiteralExpr(n) })
  private lazy val numericLiteralFirstExpr: PackratParser[NumericExpr] = (numericLiteralExpr | numericExpr)
  private lazy val unaryPosExpr: PackratParser[NumericExpr] = ("+" ~> numericLiteralFirstExpr)
  private lazy val unaryNegExpr: PackratParser[UnaryNegExpr] = ("-" ~> numericLiteralFirstExpr) ^^ { case expr => UnaryNegExpr(expr) }
  private lazy val parenthesizedExpr: PackratParser[NumericExpr] = (("(" ~> numericExpr) <~ ")") ^^ { case expr => expr }

  private def mkExpr(op: String, e1: NumericExpr, e2: NumericExpr) = {
    op match {
      case "+" => AddExpr(e1, e2)
      case "-" => SubExpr(e1, e2)
      case "*" => MulExpr(e1, e2)
      case "/" => DivExpr(e1, e2)
    }
  }

  private lazy val numericExpr: PackratParser[NumericExpr] =
    (term ~ termFrag) ^^ {
      case t1 ~ ts => ts.foldLeft(t1) { case (acc, op ~ t2) => mkExpr(op, acc, t2) }
    }
  private lazy val term: PackratParser[NumericExpr] =
    factor ~ factorFrag ^^ {
      case t1 ~ ts => ts.foldLeft(t1) { case (acc, op ~ t2) => mkExpr(op, acc, t2) }
    }
  private lazy val termFrag: PackratParser[Seq[String ~ NumericExpr]] = (("+" | "-") ~ term)*
  private lazy val factor: PackratParser[NumericExpr] = numericLiteralExpr | parenthesizedExpr | unaryPosExpr | unaryNegExpr
  private lazy val factorFrag: PackratParser[Seq[String ~ NumericExpr]] = (("*" | "/") ~ factor)*

  private lazy val booleanExpression: PackratParser[BooleanExpr] = (trueKeyword ^^^ BooleanLiteralExpr(true)) | (falseKeyword ^^^ BooleanLiteralExpr(false))

  private lazy val txFrag = (((dateFrag | isoDateFrag) ~ (annotation?) ~ (payee?)) <~ eol) ~ (eolComment*) ~ (post+) ^^ {
    case date ~ annotationOpt ~ optPayee ~ optComment ~ posts =>
      val annotationStrOpt = annotationOpt.map(_.mkString(""))
      Transaction(date, posts, annotationStrOpt, optPayee, optComment.flatten)
  }
  private lazy val annotation = (("(" ~> (ident | numericLit)+) <~ ")")
  private lazy val payee = ((allButEOL)+) ^^ { case x => x.mkString(" ") }
  private lazy val post: PackratParser[Post] = (accountName ~ opt(numericExpr) ~ eolComment) ^^ {
    case name ~ amount ~ commentOpt => Post(name, amount, commentOpt)
  }

  lazy val isoDateFrag = ((((integer <~ "-") ~ integer) <~ "-") ~ integer) ^? ({
    case y ~ m ~ d if (isValidDate(y, m, d)) =>
      Date(y, m, d)
  }, { case y ~ m ~ d => List(y, m, d).mkString("-") + " is not a valid ISO 8601 date" })

  lazy val dateFrag = ((((integer <~ "/") ~ (integer | ident)) <~ "/") ~ integer) ^? ({
    case y ~ (m: Int) ~ d if (isValidDate(y, m, d)) =>
      Date(y, m, d)
    case y ~ (m: String) ~ d if (isValidDate(y, m, d)) =>
      Date(y, Helper.getMonthNumber(m).get, d)
  }, { case y ~ m ~ d => List(y, m, d).mkString("/") + " is not a valid calendar date" })

  private def isValidDate(y: Int, m: String, d: Int): Boolean = {
    Helper.getMonthNumber(m).isDefined && isValidDate(y, Helper.getMonthNumber(m).get, d)
  }

  private def isValidDate(y: Int, m: Int, d: Int): Boolean = {
    try {
      java.time.LocalDate.of(y, m, d)
      true
    } catch {
      case _: java.time.DateTimeException => false
    }
  }
}

object ParserHelper {
  import scala.util.parsing.input.PagedSeqReader
  import scala.collection.immutable.PagedSeq

  def reader(s: String) = new PagedSeqReader(PagedSeq.fromStrings(collection.immutable.Seq(s)))
  def mkScanner(r: PagedSeqReader) = new AbandonParser.lexical.Scanner(r)
  def scanner(s: String) = mkScanner(reader(s))
}
