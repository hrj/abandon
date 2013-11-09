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
        Right(Settings(inputs, Seq(allReport), ReportOptions(Nil), Nil, None))
    }
  }

  def makeSettings(configFileName: String) = {
    val file = new java.io.File(configFileName)
    if (file.exists) {
      try {
        val config = ConfigFactory.parseFile(file).resolve()
        val inputs = config.getStringList("inputs").asScala.map(Processor.mkRelativeFileName(_, configFileName))
        val reports = config.getConfigList("reports").asScala.map(makeReportSettings(_))
        val reportOptions = config.optional("reportOptions") { _.getConfig(_) }
        val isRight = reportOptions.map(_.optional("isRight") { _.getStringList(_).asScala }).flatten.getOrElse(Nil)
        val exportConfigs = config.optional("exports"){_.getConfigList(_).asScala}.getOrElse(Nil)
        val exports = exportConfigs.map(makeExportSettings)
        Right(Settings(inputs, reports, ReportOptions(isRight), exports, Some(file)))
      } catch {
        case e: ConfigException.Missing => Left(e.getMessage)
        case e: ConfigException.Parse   => Left(e.getMessage)
      }
    } else {
      Left("Config file not found: " + configFileName)
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

case class Settings(
  inputs: Seq[String],
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

