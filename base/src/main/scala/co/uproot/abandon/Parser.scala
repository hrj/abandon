package co.uproot.abandon

import scala.language.postfixOps
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.input.PagedSeqReader
import scala.collection.immutable.PagedSeq

object AbandonLexer extends StdLexical with ImplicitConversions {
  import scala.util.parsing.input.CharArrayReader.EofCh
  override def token: Parser[Token] =
    //( '\"' ~ rep(charSeq | letter) ~ '\"' ^^ lift(StringLit)
    (string ^^ StringLit
      | identChar ~ rep(identChar | digit | '\'') ^^ { case first ~ rest => processIdent(first :: rest mkString "") }
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
  override def comment = ';' ~> rep(chrExcept(EofCh, '\n', ';')) ^^ { case chars => chars.mkString }

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

class AbandonParser(inputPathOpt: Option[String]) extends StandardTokenParsers with PackratParsers {
  override val lexical = AbandonLexer
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
  lexical.delimiters ++= List("+", "-", "*", "/", "=", "(", ")", ":", ",", "&", ".", "?", ">", "<", "{", "}")

  val nameSeparator = ":"

  private lazy val integer = numericLit ^^ { case x => x.toInt }
  private lazy val number: PackratParser[BigDecimal] = accept("number", { case lexical.NumericLit(n) => BigDecimal(n) })
  private lazy val eol = accept("<eol>", { case lexical.EOL => })
  private lazy val comment = accept("<comment>", { case lexical.CommentToken(c) => c })
  private lazy val comments = comment*
  private lazy val anyEol = comments ~ eol
  private lazy val allButEOL: PackratParser[String] = accept("any", {
    case t: lexical.Token if !t.isInstanceOf[lexical.EOL.type] && !t.isInstanceOf[lexical.ErrorToken] => t.chars
  })
  private lazy val allUntilEOL = (allButEOL+) ^^ { case tokens => tokens.mkString("") }
  private lazy val stringOrAllUntilEOL = stringLit | allUntilEOL

  private lazy val fragSeparators = anyEol*
  private def line[T](p: Parser[T]): Parser[T] = p <~ ((comments ~ eol)*)

  // End of line comments
  private def eolComments = comments <~ eol

  lazy val abandon: Parser[Scope] = abandon(None)
  def abandon(parentScopeOpt: Option[Scope]): Parser[Scope] = phrase(frags) ^^ { case entries => ParserHelper.fixupScopeParents(Scope(entries, None), parentScopeOpt) }

  private lazy val scopeBlock: Parser[Scope] = ("{" ~> frags) <~ "}"  ^^ { case entries => Scope(entries, None) }

  lazy val accountName: Parser[AccountName] = rep1sep(ident, nameSeparator) ^^ { case path => AccountName(path) }

  private lazy val frags = (fragSeparators ~> repsep(fragment, fragSeparators)) <~ fragSeparators

  private lazy val fragment: Parser[ASTEntry] = (scopeBlock | compactTxFrag | txFrag | defFrag | accountDefFrag | includeFrag | payeeDefFrag | tagDefFrag)
  private lazy val includeFrag = (includeKeyword ~> fileName) ^^ { case name => IncludeDirective(name) }
  private lazy val payeeDefFrag = (payeeKeyword ~> stringOrAllUntilEOL) ^^ { case payee => PayeeDef(payee.mkString("")) }
  private lazy val tagDefFrag = (tagKeyword ~> stringOrAllUntilEOL) ^^ { case tag => TagDef(tag.mkString("")) }

  private lazy val fileName = stringOrAllUntilEOL

  private lazy val accountDefFrag = ((accountKeyword ~> accountName) <~ anyEol) ~ accountDefDetails ^^ {
    case name ~ details => AccountDeclaration(name, details)
  }
  private lazy val accountDefDetails = keyValuePairs
  private lazy val keyValuePairSingleton = (currentPosition ~ ident) ^^ { case pos ~ k => (k, BooleanLiteralExpr(true)(Some(pos))) }
  private lazy val keyValuePairBoolean = ((ident <~ ":") ~ booleanExpr) ^^ { case k ~ v => (k, v) }
  private lazy val keyValuePair = keyValuePairSingleton | keyValuePairBoolean
  private lazy val keyValuePairs = ((keyValuePair <~ anyEol)*) ^^ { case s => s.toMap }

  private lazy val currentPosition = new Parser[InputPosition] {
    def apply(in:Input) = {
      Success(InputPosition(inputPathOpt, in.pos), in)
    }
  }

  private lazy val defFrag = currentPosition ~ ((((defKeyword ~> ident) ~ (arguments?)) <~ "=") ~ expression) ^^ {
    case pos ~ (name ~ arg ~ expr) => Definition(pos, name, arg.getOrElse(Nil), expr)
  }

  private lazy val arguments = ("(" ~> rep1sep(ident, ",")) <~ ")"
  private lazy val expression: PackratParser[Expr] = (numericExpr ||| booleanExpr ||| stringExpr)

  private def mkFunctionExpr(exprParser: Parser[Expr]) = {
    val zeroArgFunctionExpr = (currentPosition ~ ident ^^ { case pos ~ x => IdentifierExpr(x)(Some(pos)) })
    val multiArgFunctionExpr = currentPosition ~ (((ident <~ "(") ~ rep1sep(exprParser, ",")) <~ ")") ^^ {
      case pos ~ (x ~ es) => FunctionExpr(x, es, Some(pos))
      }
    multiArgFunctionExpr ||| zeroArgFunctionExpr
  }

  private lazy val functionExpr = mkFunctionExpr(expression)

  lazy val numericParser: Parser[Expr] = phrase(numericExpr)

  private lazy val numericLiteralExpr: PackratParser[Expr] = (currentPosition ~ number ^^ { case pos ~ n => NumericLiteralExpr(n)(Some(pos)) })
  private lazy val numericLiteralFirstExpr: PackratParser[Expr] = (numericLiteralExpr | numericExpr)
  private lazy val unaryPosExpr: PackratParser[Expr] = ("+" ~> numericLiteralFirstExpr)
  private lazy val unaryNegExpr: PackratParser[UnaryNegExpr] = currentPosition ~ ("-" ~> numericLiteralFirstExpr) ^^ {
    case pos ~ expr => UnaryNegExpr(expr)(Some(pos))
  }
  private lazy val parenthesizedExpr: PackratParser[Expr] = (("(" ~> numericExpr) <~ ")") ^^ { case expr => expr }

  private lazy val ternaryIfExpr: PackratParser[Expr] = currentPosition ~ (booleanExpr <~ "?") ~ (expression <~ ":") ~ expression ^^ {
    case pos ~ be ~ e1 ~ e2 => IfExpr(be, e1, e2)(Some(pos))
  }

  private def mkExpr(op: String, e1: Expr, e2: Expr, pos: InputPosition) = {
    op match {
      case "+" => AddExpr(e1, e2)(Some(pos))
      case "-" => SubExpr(e1, e2)(Some(pos))
      case "*" => MulExpr(e1, e2)(Some(pos))
      case "/" => DivExpr(e1, e2)(Some(pos))
    }
  }

  private lazy val numericExpr: PackratParser[Expr] =
    currentPosition ~ (term ~ termFrag) ^^ {
      case pos ~ (t1 ~ ts) => ts.foldLeft(t1) { case (acc, op ~ t2) => mkExpr(op, acc, t2, pos) }
    }

  private lazy val term: PackratParser[Expr] =
    currentPosition ~ factor ~ factorFrag ^^ {
      case pos ~ t1 ~ ts => ts.foldLeft(t1) { case (acc, op ~ t2) => mkExpr(op, acc, t2, pos) }
    }

  private lazy val termFrag: PackratParser[Seq[String ~ Expr]] = (("+" | "-") ~ term)*
  private lazy val factor: PackratParser[Expr] = ternaryIfExpr | functionExpr | numericLiteralExpr | parenthesizedExpr | unaryPosExpr | unaryNegExpr
  private lazy val factorFrag: PackratParser[Seq[String ~ Expr]] = (("*" | "/") ~ factor)*

  private lazy val stringLitExpr = (currentPosition ~ stringLit) ^^ {case pos ~ lit => StringLiteralExpr(lit)(Some(pos))}
  private lazy val stringExpr: PackratParser[Expr] =  stringLitExpr | functionExpr
  private lazy val booleanExpr: PackratParser[Expr] =  conditionExpr | booleanLiteralExpr | functionExpr
  private lazy val trueExpr = currentPosition ~ trueKeyword ^^ { case pos ~ lit => BooleanLiteralExpr(true)(Some(pos))}
  private lazy val falseExpr = currentPosition ~ falseKeyword ^^ { case pos ~ lit => BooleanLiteralExpr(false)(Some(pos))}
  private lazy val booleanLiteralExpr = trueExpr | falseExpr
  private lazy val conditionExpr = currentPosition ~ (numericExpr ~ comparisonExpr ~ numericExpr) ^^ {
    case pos ~ (e1 ~ op ~ e2) => ConditionExpr(e1, op, e2)(Some(pos))
  }
  private lazy val comparisonExpr: PackratParser[String] = ((">" | "<" | "=") ~ "=" ^^ {case (o1 ~ o2) => o1 + o2}) | ">" | "<" 

  private lazy val compactTxFrag = currentPosition ~ ("." ~> dateExpr ~ accountName ~ numericExpr ~ eolComments) ^^ {
    case pos ~ (date ~ accountName ~ amount ~ comments) =>
      Transaction(pos, date, List(Post(accountName, Option(amount), comments.headOption)), None, None,
        if (comments.isEmpty) Nil else comments.tail)
  }

  private lazy val txFrag = currentPosition ~ (((dateExpr ~ (annotation?) ~ (payee?)) <~ eol) ~ (eolComments*) ~ (post+)) ^^ {
    case pos ~ (date ~ annotationOpt ~ optPayee ~ comments ~ posts) =>
      val annotationStrOpt = annotationOpt.map(_.mkString(""))
      Transaction(pos, date, posts, annotationStrOpt, optPayee,
        if (comments.isEmpty) Nil else List(comments.flatten.mkString(";")))
  }

  private lazy val annotation = (("(" ~> (ident | numericLit)+) <~ ")")

  private lazy val payee = ((allButEOL)+) ^^ { case x => x.mkString(" ") }

  private lazy val post: PackratParser[Post] = (accountName ~ opt(numericExpr) ~ eolComments) ^^ {
    case name ~ amount ~ comments => Post(name, amount,
      if (comments.isEmpty) None else Some(comments.mkString(";")))
  }

  lazy val dateExpr = dateFrag | isoDateFrag

  lazy val inclusive = ident ^? {
    case "inclusive" => true
    case "exclusive" => false
  }

  lazy val dateBoundExpr = (dateExpr ~ inclusive) ^^ { case date ~ inclusive => DateBound(date, inclusive) }

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

  def scanner(s:String) = {
    val reader= new PagedSeqReader(PagedSeq.fromStrings(collection.immutable.Seq(s)))
    new lexical.Scanner(reader)
  }

  def scannerFromFile(filePath:String) = {
    val reader= new PagedSeqReader(PagedSeq.fromFile(filePath))
    new lexical.Scanner(reader)
  }
}


object ParserHelper {

  val parser = new AbandonParser(None)

  def scanner(s: String) = parser.scanner(s)

  def fixupScopeParents(s:Scope, parentOpt: Option[Scope]): Scope = {
    Scope(s.entries.map( {
      case c:Scope => fixupScopeParents(c, Option(s))
      case x => x
    }), parentOpt)
  }
}
