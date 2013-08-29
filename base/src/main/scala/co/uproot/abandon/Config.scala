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
        val allReport = BalanceReportSettings("All Balances", None, true)
        Right(Settings(inputs, Seq(allReport), ReportOptions(Nil)))
    }
  }

  def makeSettings(configFileName: String) = {
    val file = new java.io.File(configFileName)
    if (file.exists) {
      val config = ConfigFactory.parseFile(file).resolve()
      try {
        val inputs = config.getStringList("inputs").asScala.map(Processor.mkRelativeFileName(_, configFileName))
        val reports = config.getConfigList("reports").asScala.map(makeReportSettings(_))
        val reportOptions = config.optional("reportOptions") { _.getConfig(_) }
        val isRight = reportOptions.map(_.optional("isRight") { _.getStringList(_).asScala }).flatten.getOrElse(Nil)
        Right(Settings(inputs, reports, ReportOptions(isRight)))
      } catch {
        case e: ConfigException.Missing => Left(e.getMessage)
      }
    } else {
      Left("Config file not found: " + configFileName)
    }
  }

  def makeReportSettings(config: Config) = {
    val title = config.getString("title")
    val reportType = config.getString("type")
    val accountMatch = config.optional("accountMatch") { _.getStringList(_).asScala }
    reportType match {
      case "balance" =>
        val showZeroAmountAccounts = config.optional("showZeroAmountAccounts") {_.getBoolean(_)}.getOrElse(false)
        BalanceReportSettings(title, accountMatch, showZeroAmountAccounts)
      case "register" =>
        RegisterReportSettings(title, accountMatch)
      case "book" =>
        val account = config.getString("account")
        BookReportSettings(title, account)
    }
  }
}

case class Settings(inputs: Seq[String], reports: Seq[ReportSettings], reportOptions: ReportOptions)

abstract class ReportSettings(val title:String, val accountMatch: Option[Seq[String]]) {
  def isAccountMatching(name: String) = {
    accountMatch.map(patterns => patterns.exists(name matches _)).getOrElse(true)
  }
}

case class BalanceReportSettings(
  _title: String,
  _accountMatch: Option[Seq[String]],
  showZeroAmountAccounts: Boolean) extends ReportSettings(_title, _accountMatch) {
}

case class RegisterReportSettings(_title: String, _accountMatch: Option[Seq[String]]) extends ReportSettings(_title, _accountMatch) {
}

case class BookReportSettings(_title: String, account: String) extends ReportSettings(_title, Some(Seq(account))) {
}

case class ReportOptions(isRight: Seq[String])

