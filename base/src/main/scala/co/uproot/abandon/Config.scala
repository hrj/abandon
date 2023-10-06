package co.uproot.abandon

import java.nio.file.{Path, Paths}
import java.time.format.DateTimeParseException

import com.typesafe.config.{Config, ConfigException, ConfigFactory}
import org.rogach.scallop.ScallopConf
import org.rogach.scallop.exceptions.{Help, ScallopException, Version}
import org.rogach.scallop.stringListConverter
import org.rogach.scallop.stringPropsConverter
import org.rogach.scallop.flagConverter
import org.rogach.scallop.stringConverter

import scala.jdk.CollectionConverters._

/**
  * Holder for application's version identification
  * @param id (combined) version information as string
  */
case class VersionId(id: String)

class AbandonCLIConf(arguments: Seq[String]) extends ScallopConf(arguments) {
  override def onError(e: Throwable): Unit = e match {
    case ex: ScallopException => {
      printHelp()
      throw e
    }
    case Help("") => {
      builder.printHelp()
      throw e
    }
    case Help(subname) => {
      builder.findSubbuilder(subname).get.printHelp()
      throw e
    }
    case Version => {
      builder.vers.foreach(println)
      throw e
    }
    case other => super.onError(other)
  }

  val inputs = opt[List[String]]("input", short = 'i')
  val reports = opt[List[String]]("report", short = 'r')
  val config = opt[String]("config", short = 'c')
  val filters = propsLong[String]("filter", descr="Transaction filters", keyName=" name")
  val unversioned = opt[Boolean]("unversioned", short = 'X')
  val webStartDate = opt[String]("web-start-date", short = 'w', descr = "start date used for web reports. specifying this starts the web server")
  val quiet = opt[Boolean]("quiet", short = 'q')
  val version = opt[Boolean]("version", noshort = true)
  val help = opt[Boolean]("help", short = 'h')
}

object SettingsHelper {
  implicit class ConfigHelper(val config: Config) extends AnyVal {
    def optional[T](path: String)(f: (Config, String) => T) = {

      if (config.hasPath(path)) {
        Some(f(config, path))
      } else {
        None
      }
    }
  }

  def createTxnFilter(key: String, value: String): TransactionFilter = {
    def makeDate(date: String) = {
      val jDate = try {
        java.time.LocalDate.parse(date,
          java.time.format.DateTimeFormatter.ISO_DATE)
      } catch {
        case ex: DateTimeParseException =>
          throw new SettingsError("Filters" + "\n" +
            "   Invalid date: " + date + "\n" +
            "   Reason: " + ex.getMessage)
      }
      Date(jDate.getYear, jDate.getMonthValue, jDate.getDayOfMonth)
    }

    (key, value) match {
      case (key, value) if (key == "onOrAfter") => {
        OnOrAfterDateTxnFilter(makeDate(value))
      }
      case (key, value) if (key == "before") => {
        BeforeDateTxnFilter(makeDate(value))
      }
      case (key, value) if (key == "payee") => {
        PayeeTxnFilter(value)
      }
      case (key, value) if (key == "account") => {
        AccountNameTxnFilter(value)
      }
      case (key, value) if (key == "annotation") => {
        AnnotationTxnFilter(value)
      }
      case _ => {
        throw new SettingsError("Unknown filter: " + key)
      }
    }
  }

  def getCompleteSettings(cliConf: AbandonCLIConf, version: String): Either[String, Settings] = {
    val configOpt = cliConf.config.toOption
    val versionId = if (cliConf.unversioned.getOrElse(false)) {
      None
    } else {
      Option(VersionId(version))
    }
    val quiet = cliConf.quiet.getOrElse(false)
    val txnFilters =
      if (cliConf.filters.isEmpty) {
        None
      }else {
        val txnfs: Seq[TransactionFilter] =
          cliConf.filters.map({case (k,v) => createTxnFilter(k, v)}).toSeq
        Option(ANDTxnFilterStack(txnfs))
      }

    configOpt match {
      case Some(configFileName) =>
        makeSettings(configFileName, versionId,  quiet, txnFilters)
      case _ =>
        val inputs = cliConf.inputs.toOption.getOrElse(Nil)
        val allReport = BalanceReportSettings("All Balances", None, Nil, true)
        Right(Settings(inputs, Nil, Nil, Seq(allReport), ReportOptions(Nil), Nil, None, quiet, versionId, txnFilters))
    }
  }

  implicit class ConfigHelper2(val config: Config) {
    def optConfig(name: String) = {
      config.optional(name) { _.getConfig(_) }
    }
    def optConfigList(name: String) = {
      config.optional(name) { _.getConfigList(_).asScala.toSeq }
    }
    def optStringList(name: String) = {
      config.optional(name) { _.getStringList(_).asScala.toSeq }
    }
  }

  def makeSettings(configFileName: String, version: Option[VersionId], quiet: Boolean, txnFiltersCLI: Option[TxnFilterStack]) = {
    def handleInput(input: String, confPath: String): List[String] = {
      val parentPath = Processor.mkParentDirPath(confPath)
      if (input.startsWith("glob:")) {
        FileUtils.globListFiles(input, parentPath)
      } else if (input.startsWith("regex:")) {
        FileUtils.regexListFiles(input, parentPath)
      } else {
        List(Processor.mkRelativeFileName(input, confPath))
      }
    }

    val file = new java.io.File(configFileName)
    if (file.exists) {
      try {
        val config = ConfigFactory.parseFile(file).resolve()
        val inputs = config.getStringList("inputs").asScala.flatMap(handleInput(_, configFileName)).toSeq.sorted
        val reports = config.getConfigList("reports").asScala.toSeq.map(makeReportSettings(_))
        val reportOptions = config.optConfig("reportOptions")
        val isRight = reportOptions.flatMap(_.optStringList("isRight")).getOrElse(Nil)
        val exportConfigs = config.optConfigList("exports").getOrElse(Nil)
        val exports = exportConfigs.map(makeExportSettings(_, version))
        val accountConfigs = config.optConfigList("accounts").getOrElse(Nil)
        val accounts = accountConfigs.map(makeAccountSettings)
        val eodConstraints = config.optConfigList("eodConstraints").getOrElse(Nil).map(makeEodConstraints(_))

       /*
        * filters, precedence
        *  - conf none, cli none => None
        *  - conf none, cli some => cli
        *  - conf some, cli some => cli
        */
        val txnFilters = txnFiltersCLI match {
          case Some(txnfs) => Option(txnfs)
          case None =>
            try {
              val txnfs = config.getStringList("filters").asScala.toSeq.map(s => s.split("=", 2)).
                  map({ case Array(k, v) => createTxnFilter(k, v) })
              Option(ANDTxnFilterStack(txnfs))
            } catch {
              case e: ConfigException.Missing => None
            }
        }
        val dateConstraints = config.optConfigList("dateConstraints").getOrElse(Nil).map(makeDateRangeConstraint(_))
        Right(Settings(inputs, eodConstraints ++ dateConstraints, accounts, reports, ReportOptions(isRight), exports, Some(file), quiet, version, txnFilters))
      } catch {
        case e: ConfigException => Left(e.getMessage)
      }
    } else {
      Left(s"Config file not found: $configFileName. PWD is ${System.getProperty("user.dir")}")
    }
  }

  // For now we only support simple constraints
  def makeEodConstraints(config: Config) = {
    val expr = config.getString("expr")
    config.getString("constraint") match {
      case "positive" => PositiveConstraint(expr)
      case "negative" => NegativeConstraint(expr)
      case "equals" => {
        val onDateStr = config.getString("onDate")
        val accName = config.getString("account")
        val onDate = parseDate(config, onDateStr)
        val expression = parseExpr(config, expr)
        EqualsConstraint(onDate, accName, expression)
      }
    }
  }

  private def parseExpr(config: Config, exprStr: String) = {
    ParserHelper.parser.numericParser(ParserHelper.parser.scanner(exprStr)) match {
      case ParserHelper.parser.Success(expr, _) => expr
      case ParserHelper.parser.NoSuccess(_, _) =>
        throw new ConfigException.BadValue(config.origin, "onDate", "expected a date")
      case _ =>
        throw new ConfigException.BadValue(config.origin, "onDate", "expected a date")
    }
  }

  private def parseDate(config: Config, dateStr: String) = {
    Helper.parseDate(dateStr) match {
      case Some(date) => date
      case None =>
        throw new ConfigException.BadValue(config.origin, "onDate", "expected a date")
    }
  }

  private def getDateBound(name: String, config: Config) : Option[DateBound] = {
    config.optional(name) { _.getString(_) } match {
      case Some(valueStr) =>
        val parseResult = ParserHelper.parser.dateBoundExpr(ParserHelper.scanner(valueStr))
        parseResult match {
          case ParserHelper.parser.Success(dateBound, _) => Option(dateBound)
          case ParserHelper.parser.NoSuccess(_, _) =>
            throw new ConfigException.BadValue(config.origin, name, "expected a date bound of the form: <date> <inclusive|exclusive>")
          case _ => ???
        }
      case None => None
    }
  }

  private def makeDateRangeConstraint(config: Config): DateRangeConstraint = {
    DateRangeConstraint(getDateBound("from", config), getDateBound("to", config))
  }

  def makeReportSettings(config: Config) = {
    val title = config.getString("title")
    val reportType = config.getString("type")
    val accountMatch = config.optional("accountMatch") { _.getStringList(_).asScala.toSeq }
    val outFiles = config.optional("outFiles") { _.getStringList(_).asScala.toSeq }.getOrElse(Nil)
    reportType match {
      case "balance" =>
        val showZeroAmountAccounts = config.optional("showZeroAmountAccounts") { _.getBoolean(_) }.getOrElse(false)
        BalanceReportSettings(title, accountMatch, outFiles, showZeroAmountAccounts)
      case "register" =>
        val groupBy = GroupBy(config.optional("groupBy"){_.getString(_)})
        RegisterReportSettings(title, accountMatch, outFiles, groupBy)
      case "book" =>
        val account = config.getString("account")
        BookReportSettings(title, account, outFiles)
      case _ =>
        val message = s"Found '$reportType'; expected 'balance', 'register' or 'book'."
        throw new ConfigException.BadValue(config.origin, "type", message)
    }
  }

  def makeExportSettings(config: Config, version: Option[VersionId]) = {
    val exportTypeStr = config.getString("type")

    val exportFormatStr = config.getString("format")
    val exportFormat = exportFormatStr match {
      case "ledger" => LedgerType
      case "xml" => XMLType
      case _ =>
        val message = s"In '$exportTypeStr' export found format '$exportFormatStr'; expected 'ledger' or 'xml'."
        throw new ConfigException.BadValue(config.origin, "type", message)
    }

    val accountMatch = config.optional("accountMatch") { _.getStringList(_).asScala.toSeq }
    val outFiles = config.optional("outFiles") { _.getStringList(_).asScala.toSeq }.getOrElse(Nil)


      exportTypeStr match {
      case "journal" =>
        val accountMatch = config.optional("accountMatch") { _.getStringList(_).asScala.toSeq }
        JournalExportSettings(XMLType, accountMatch, outFiles, version)
      case "balance" =>
        val showZeroAmountAccounts = config.optional("showZeroAmountAccounts") { _.getBoolean(_) }.getOrElse(false)
        val closureConfig = config.optConfigList("closures").getOrElse(Nil)
        val closure = closureConfig.map(makeClosureSettings)
        BalanceExportSettings(exportFormat, accountMatch, outFiles, version, showZeroAmountAccounts, closure)
      case _ =>
        val message = s"Found '$exportTypeStr'; expected 'journal' or 'balance'."
        throw new ConfigException.BadValue(config.origin, "type", message)
    }
  }

  def makeClosureSettings(config: Config) = {
    val sources = config.getStringList("sources").asScala.toSeq
    val destination = config.getString("destination")
    ClosureExportSettings(sources, destination)
  }
  def makeAccountSettings(config: Config) = {
    val name = config.getString("name")
    val alias = config.optional("alias") { _.getString(_) }
    AccountSettings(ASTHelper.parseAccountName(name), alias)
  }

  def ensureInputProtection(inputs: Set[String], settings: Settings): Unit = {
    val inputFiles = inputs map Processor.mkPath
    val outputFiles = settings.getOutputFiles
    val overwritten = inputFiles intersect outputFiles
    if (overwritten.nonEmpty) {
      throw new SettingsError("Your configuration would overwrite input files:\n" + overwritten.mkString("\n"))
    }
  }
}

abstract class Constraint {
  def check(appState: AppState): Boolean
}

trait SignChecker {
  val accName: String
  val signStr: String
  val correctSign: (BigDecimal) => Boolean

  def check(appState: AppState) = {
    val posts = appState.accState.posts.filter(_.name.fullPathStr == accName)
    val dailyDeltas = posts.groupBy(_.date.toInt).view.mapValues(s => Helper.sumDeltas(s))

    var acc = Helper.Zero
    dailyDeltas.toSeq.sortBy(_._1).foreach {
      case (dayInt, delta) =>
        acc += delta
        if (!correctSign(acc)) {
          throw new ConstraintError(s"$accName was not $signStr on ${Date.fromInt(dayInt)}. Was $acc")
        }
    }
    true
  }

}

case class PositiveConstraint(val accName: String) extends Constraint with SignChecker {
  val correctSign = (x: BigDecimal) => x >= Helper.Zero
  val signStr = "positive"
}

case class NegativeConstraint(val accName: String) extends Constraint with SignChecker {
  val correctSign = (x: BigDecimal) => x <= Helper.Zero
  val signStr = "negative"
}

case class EqualsConstraint(val onDate: Date, val accName: String, val expr: Expr) extends Constraint {
  def check(appState: AppState) = {
    val posts = appState.accState.posts.filter(post => post.name.fullPathStr == accName && isNotLaterThan(post, onDate))
    val sum = Helper.sumDeltas(posts)

    val rootScope = Scope(Nil, None)
    val context = new EvaluationContext(rootScope, Nil)
    val exprValue = context.evaluateBD(expr)
    if (sum != exprValue) {
      throw new ConstraintError(s"$accName was not equal to ${exprValue} on $onDate. Was $sum. Difference = ${exprValue - sum}")
    }
    true
  }

  def isNotLaterThan(post: DetailedPost, that: Date) = {
    DateOrdering.compare(post.date, that) <= 0
  }
}

case class DateBound(date: Date, inclusive: Boolean) {
  def isNotEarlierThan(that: Date) = {
    if (inclusive) {
      DateOrdering.compare(this.date, that) > 0
    } else {
      DateOrdering.compare(this.date, that) >= 0
    }
  }

  def isNotLaterThan(that: Date) = {
    if (inclusive) {
      DateOrdering.compare(this.date, that) < 0
    } else {
      DateOrdering.compare(this.date, that) <= 0
    }
  }
}

case class DateRangeConstraint(dateFromOpt: Option[DateBound], dateToOpt: Option[DateBound]) extends Constraint {
  override def check(appState: AppState): Boolean = {
    appState.accState.postGroups.find(postGroup => {
      val date = postGroup.txn.date

      val fromFails = dateFromOpt.map(_.isNotEarlierThan(date)).getOrElse(false)
      val toFails = dateToOpt.map(_.isNotLaterThan(date)).getOrElse(false)

      fromFails || toFails
    }).foreach(postGroup => {
      throw new ConstraintPosError(
        s"Transaction dated ${postGroup.txn.date.formatISO8601Ext} is not in the range of " +
        s"[${dateFromOpt.getOrElse("...")}, ${dateToOpt.getOrElse("...")}]. ",
        postGroup.txn.pos
      )
    })

    true
  }
}

class SettingsError(msg: String) extends RuntimeException(msg)

case class Settings(
  inputs: Seq[String],
  constraints: Seq[Constraint],
  accounts: Seq[AccountSettings],
  reports: Seq[ReportSettings],
  reportOptions: ReportOptions,
  exports: Seq[ExportSettings],
  configFileOpt: Option[java.io.File],
  quiet: Boolean,
  version: Option[VersionId],
  txnFilters: Option[TxnFilterStack]) {
  def writesToScreen(outFiles: Seq[String]): Boolean = {
    outFiles.contains("-") || outFiles.isEmpty
  }
  def getConfigRelativePath(path: String): String = {
    configFileOpt.map(configFile => Processor.mkRelativeFileName(path, configFile.getAbsolutePath)).getOrElse(path)
  }
  def getConfigRelativePaths(paths: Seq[String]): Seq[String] = {
    paths filterNot (_ equals "-") map getConfigRelativePath
  }
  def getOutputFiles: Set[Path] = {
    (reports.map(_.outFiles) ++ exports.map(_.outFiles)).flatMap(getConfigRelativePaths).map(Processor.mkPath).toSet
  }
}

trait AccountMatcher {
  val accountMatch: Option[Seq[String]]
  def isAccountMatching(name: String) = {
    accountMatch.map(patterns => patterns.exists(name matches _)).getOrElse(true)
  }
}

sealed trait OutputType
object BalanceType extends OutputType
object JournalType extends OutputType

sealed trait FormatType
object LedgerType extends FormatType
object XMLType extends FormatType


abstract class ReportSettings(val title: String, val accountMatch: Option[Seq[String]], val outFiles: Seq[String]) extends AccountMatcher {
}

abstract class ExportSettings(val accountMatch: Option[Seq[String]], val outFiles: Seq[String], val appVersion: Option[VersionId]) extends AccountMatcher {
}

case class ClosureExportSettings(
  sources: Seq[String],
  destination: String) {
}
case class AccountSettings(
  name: AccountName,
  alias: Option[String]) {
}

case class JournalExportSettings(exportFormat: FormatType, _accountMatch: Option[Seq[String]], _outFiles: Seq[String], _appVersion: Option[VersionId]) extends ExportSettings(_accountMatch, _outFiles, _appVersion)

case class BalanceExportSettings(
  exportFormat: FormatType,
  _accountMatch: Option[Seq[String]],
  _outFiles: Seq[String],
  _appVersion: Option[VersionId],
  showZeroAmountAccounts: Boolean, closure: Seq[ClosureExportSettings]) extends ExportSettings(_accountMatch, _outFiles, _appVersion) {
}

case class BalanceReportSettings(
  _title: String,
  _accountMatch: Option[Seq[String]],
  _outFiles: Seq[String],
  showZeroAmountAccounts: Boolean) extends ReportSettings(_title, _accountMatch, _outFiles) {
}

/**
  * GroupBy selectors
  */
sealed trait GroupBy
sealed case class GroupByYear() extends GroupBy
sealed case class GroupByMonth() extends GroupBy
sealed case class GroupByDay() extends GroupBy
sealed case class GroupByIsoWeek() extends GroupBy
sealed case class GroupByIsoWeekDate() extends GroupBy

object GroupBy {
  def apply(groupBy: Option[String]): GroupBy = {
    groupBy match {
      case Some("year") => GroupByYear()
      case Some("month") => GroupByMonth()
      case Some("day") => GroupByDay()
      case Some("isoWeek") => GroupByIsoWeek()
      case Some("isoWeekDate") => GroupByIsoWeekDate()
      /* default */
      case None => GroupByMonth()
      /* Error*/
      case _ => throw new SettingsError(
        "GroupBy: unknown operator. Valid operators are: year, month, day, isoWeek, isoWeekDate")
    }
  }
}

case class RegisterReportSettings(_title: String,  _accountMatch: Option[Seq[String]], _outFiles: Seq[String], _groupBy: GroupBy) extends ReportSettings(_title, _accountMatch, _outFiles) {

  /**
    * Get groupOp, either Int or String based groupByOp
    *
    * @return Left(groupIntOp), Right(groupStrOp)
    */
  def groupOp: Either[(PostGroup) => Int, (PostGroup) => String] = {
    _groupBy match {
      /* groupIntOp */
      case GroupByDay() => Left({ case txn: PostGroup => txn.date.toInt })
      case GroupByMonth() => Left({ case txn: PostGroup => txn.date.toIntYYYYMM })
      case GroupByYear() => Left({ case txn: PostGroup => txn.date.toIntYYYY })

      /* groupStrOp */
      case GroupByIsoWeek() => Right({ case txn: PostGroup => txn.date.formatISO8601Week })
      case GroupByIsoWeekDate() => Right({ case txn: PostGroup => txn.date.formatISO8601WeekDate })
    }
  }
}

case class BookReportSettings(_title: String, account: String, _outFiles: Seq[String]) extends ReportSettings(_title, Some(Seq(account)), _outFiles) {
}

case class ReportOptions(isRight: Seq[String])
