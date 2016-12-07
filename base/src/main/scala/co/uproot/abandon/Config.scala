package co.uproot.abandon

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory

import collection.JavaConverters._
import org.rogach.scallop.ScallopConf
import SettingsHelper._
import com.typesafe.config.ConfigException
import scala.util.Try

class AbandonCLIConf(arguments: Seq[String]) extends ScallopConf(arguments) {
  val inputs = opt[List[String]]("input", short = 'i')
  val reports = opt[List[String]]("report", short = 'r')
  val config = opt[String]("config", short = 'c')
  val filters = propsLong[String]("filter", descr="Transaction filters", keyName=" name")
  val unversioned = opt[Boolean]("unversioned", short = 'X')
  val quiet = opt[Boolean]("quiet", short = 'q')
  // val trail = trailArg[String]()
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
      val jDate = java.time.LocalDate.parse(date,
          java.time.format.DateTimeFormatter.ISO_DATE)
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
        throw new RuntimeException("Unknown filter: " + key)
      }
    }
  }

  def getCompleteSettings(args: Seq[String]): Either[String, Settings] = {
    val cliConf = new AbandonCLIConf(args)
    cliConf.verify()
    val configOpt = cliConf.config.toOption
    val withoutVersion = cliConf.unversioned.getOrElse(false)
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
        makeSettings(configFileName, withoutVersion, quiet, txnFilters)
      case _ =>
        val inputs = cliConf.inputs.toOption.getOrElse(Nil)
        val allReport = BalanceReportSettings("All Balances", None, Nil, true)
        Right(Settings(inputs, Nil, Nil, Seq(allReport), ReportOptions(Nil), Nil, None, quiet, txnFilters))
    }
  }

  implicit class ConfigHelper2(val config: Config) {
    def optConfig(name: String) = {
      config.optional(name) { _.getConfig(_) }
    }
    def optConfigList(name: String) = {
      config.optional(name) { _.getConfigList(_).asScala }
    }
    def optStringList(name: String) = {
      config.optional(name) { _.getStringList(_).asScala }
    }
  }

  def makeSettings(configFileName: String, withoutVersion: Boolean, quiet: Boolean, txnFiltersCLI: Option[TxnFilterStack]) = {
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
        val reports = config.getConfigList("reports").asScala.map(makeReportSettings(_))
        val reportOptions = config.optConfig("reportOptions")
        val isRight = reportOptions.flatMap(_.optStringList("isRight")).getOrElse(Nil)
        val exportConfigs = config.optConfigList("exports").getOrElse(Nil)
        val exports = exportConfigs.map(makeExportSettings(_, withoutVersion))
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
              val txnfs = config.getStringList("filters").asScala.map(s => s.split("=", 2)).
                  map({ case Array(k, v) => createTxnFilter(k, v) })
              Option(ANDTxnFilterStack(txnfs))
            } catch {
              case e: ConfigException.Missing => None
            }
        }
        val dateConstraints = config.optConfigList("dateConstraints").getOrElse(Nil).map(makeDateRangeConstraint(_))
        Right(Settings(inputs, eodConstraints ++ dateConstraints, accounts, reports, ReportOptions(isRight), exports, Some(file), quiet, txnFilters))
      } catch {
        case e: ConfigException => Left(e.getMessage)
      }
    } else {
      Left("Config file not found: " + configFileName)
    }
  }

  // For now we only support simple constraints
  def makeEodConstraints(config: Config) = {
    val accName = config.getString("expr")
    config.getString("constraint") match {
      case "positive" => PositiveConstraint(accName)
      case "negative" => NegativeConstraint(accName)
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
    val accountMatch = config.optional("accountMatch") { _.getStringList(_).asScala }
    val outFiles = config.optional("outFiles") { _.getStringList(_).asScala }.getOrElse(Nil)
    reportType match {
      case "balance" =>
        val showZeroAmountAccounts = config.optional("showZeroAmountAccounts") { _.getBoolean(_) }.getOrElse(false)
        BalanceReportSettings(title, accountMatch, outFiles, showZeroAmountAccounts)
      case "register" =>
        RegisterReportSettings(title, accountMatch, outFiles)
      case "book" =>
        val account = config.getString("account")
        BookReportSettings(title, account, outFiles)
      case _ =>
        val message = s"Found '$reportType'; expected 'balance', 'register' or 'book'."
        throw new ConfigException.BadValue(config.origin, "type", message)
    }
  }

  def makeExportSettings(config: Config, withoutVersion: Boolean) = {
    val exportType = config.getString("type")
    val exportFormat = config.getString("format")

    val accountMatch = config.optional("accountMatch") { _.getStringList(_).asScala }
    val outFiles = config.optional("outFiles") { _.getStringList(_).asScala }.getOrElse(Nil)

    exportType match {
      case "journal" =>
        exportFormat match {
        case "ledger" =>
            val showZeroAmountAccounts = config.optional("showZeroAmountAccounts") { _.getBoolean(_) }.getOrElse(false)
            val closureConfig = config.optConfigList("closures").getOrElse(Nil)
            val closure = closureConfig.map(makeClosureSettings)
            LedgerExportSettings(accountMatch, outFiles, showZeroAmountAccounts, closure)
        case "xml" =>
            val accountMatch = config.optional("accountMatch") { _.getStringList(_).asScala }
            XmlExportSettings(JournalType, accountMatch, outFiles, withoutVersion)
        case _ =>
            val message = s"Found '$exportType', '$exportFormat'; expected 'ledger' or 'xml'."
            throw new ConfigException.BadValue(config.origin, "type", message)
      }
      case "balance" =>
        exportFormat match {
        case "ledger" =>
            val message = s"Found type: '$exportType' format: '$exportFormat'; This is not implemented."
            throw new NotImplementedError(message)
        case "xml" =>
            val accountMatch = config.optional("accountMatch") { _.getStringList(_).asScala }
            XmlExportSettings(BalanceType, accountMatch, outFiles, withoutVersion)
        case _ =>
            val message = s"Found '$exportType', '$exportFormat'; expected 'ledger' or 'xml'."
            throw new ConfigException.BadValue(config.origin, "type", message)
      }
      case _ =>
        val message = s"Found '$exportType'; expected 'journal' or 'balance'."
        throw new ConfigException.BadValue(config.origin, "type", message)
    }
  }

  def makeClosureSettings(config: Config) = {
    val sources = config.getStringList("sources").asScala
    val destination = config.getString("destination")
    ClosureExportSettings(sources, destination)
  }
  def makeAccountSettings(config: Config) = {
    val name = config.getString("name")
    val alias = config.optional("alias") { _.getString(_) }
    AccountSettings(ASTHelper.parseAccountName(name), alias)
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
    val dailyDeltas = posts.groupBy(_.date.toInt).mapValues(s => Helper.sumDeltas(s))

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

case class Settings(
  inputs: Seq[String],
  constraints: Seq[Constraint],
  accounts: Seq[AccountSettings],
  reports: Seq[ReportSettings],
  reportOptions: ReportOptions,
  exports: Seq[ExportSettings],
  configFileOpt: Option[java.io.File],
  quiet: Boolean,
  txnFilters: Option[TxnFilterStack]) {
  def getConfigRelativePath(path: String) = {
    configFileOpt.map(configFile => Processor.mkRelativeFileName(path, configFile.getAbsolutePath)).getOrElse(path)
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

abstract class ExportSettings(val accountMatch: Option[Seq[String]], val outFiles: Seq[String]) extends AccountMatcher {
}

case class ClosureExportSettings(
  sources: Seq[String],
  destination: String) {
}
case class AccountSettings(
  name: AccountName,
  alias: Option[String]) {
}
case class LedgerExportSettings(
  _accountMatch: Option[Seq[String]],
  _outFiles: Seq[String],
  showZeroAmountAccounts: Boolean, closure: Seq[ClosureExportSettings]) extends ExportSettings(_accountMatch, _outFiles) {
}

case class BalanceReportSettings(
  _title: String,
  _accountMatch: Option[Seq[String]],
  _outFiles: Seq[String],
  showZeroAmountAccounts: Boolean) extends ReportSettings(_title, _accountMatch, _outFiles) {
}

case class RegisterReportSettings(_title: String, _accountMatch: Option[Seq[String]], _outFiles: Seq[String]) extends ReportSettings(_title, _accountMatch, _outFiles) {
}

case class BookReportSettings(_title: String, account: String, _outFiles: Seq[String]) extends ReportSettings(_title, Some(Seq(account)), _outFiles) {
}

case class XmlExportSettings(exportType: OutputType, _accountMatch: Option[Seq[String]], _outFiles: Seq[String], withoutVersion: Boolean) extends ExportSettings(_accountMatch, _outFiles)

case class ReportOptions(isRight: Seq[String])
