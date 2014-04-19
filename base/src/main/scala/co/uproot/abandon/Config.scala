package co.uproot.abandon

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import collection.JavaConverters._
import com.typesafe.config.ConfigObject
import org.rogach.scallop.ScallopConf
import SettingsHelper._
import com.typesafe.config.ConfigException

class AbandonCLIConf(arguments: Seq[String]) extends ScallopConf(arguments) {
  val inputs = opt[List[String]]("input", short = 'i')
  val reports = opt[List[String]]("report", short = 'r')
  val config = opt[String]("config", short = 'c')
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

  def getCompleteSettings(args: Seq[String]) = {
    val cliConf = new AbandonCLIConf(args)
    val configOpt = cliConf.config.get
    configOpt match {
      case Some(configFileName) =>
        makeSettings(configFileName)
      case _ =>
        val inputs = cliConf.inputs.get.getOrElse(Nil)
        val allReport = BalanceReportSettings("All Balances", None, Nil, true)
        Right(Settings(inputs, Nil, Seq(allReport), ReportOptions(Nil), Nil, None))
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

  def makeSettings(configFileName: String) = {
    val file = new java.io.File(configFileName)
    if (file.exists) {
      try {
        val config = ConfigFactory.parseFile(file).resolve()
        val inputs = config.getStringList("inputs").asScala.map(Processor.mkRelativeFileName(_, configFileName))
        val reports = config.getConfigList("reports").asScala.map(makeReportSettings(_))
        val reportOptions = config.optConfig("reportOptions")
        val isRight = reportOptions.map(_.optStringList("isRight")).flatten.getOrElse(Nil)
        val exportConfigs = config.optConfigList("exports").getOrElse(Nil)
        val exports = exportConfigs.map(makeExportSettings)
        val eodConstraints = config.optConfigList("eodConstraints").getOrElse(Nil).map(makeEodConstraints(_))
        Right(Settings(inputs, eodConstraints, reports, ReportOptions(isRight), exports, Some(file)))
      } catch {
        case e: ConfigException.Missing => Left(e.getMessage)
        case e: ConfigException.Parse   => Left(e.getMessage)
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
    }
  }

  def makeExportSettings(config: Config) = {
    val accountMatch = config.optional("accountMatch") { _.getStringList(_).asScala }
    val outFiles = config.optional("outFiles") { _.getStringList(_).asScala }.getOrElse(Nil)
    ExportSettings(accountMatch, outFiles)
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
    val txns = appState.accState.txns.filter(_.name.fullPathStr == accName)
    val dailyDeltas = txns.groupBy(_.date.toInt).mapValues(s => Helper.sumDeltas(s))

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

case class Settings(
  inputs: Seq[String],
  eodConstraints: Seq[Constraint],
  reports: Seq[ReportSettings],
  reportOptions: ReportOptions,
  exports: Seq[ExportSettings],
  configFileOpt: Option[java.io.File]) {
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

abstract class ReportSettings(val title: String, val accountMatch: Option[Seq[String]], val outFiles: Seq[String]) extends AccountMatcher {
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

case class ExportSettings(accountMatch: Option[Seq[String]], outFiles: Seq[String]) extends AccountMatcher

case class ReportOptions(isRight: Seq[String])

