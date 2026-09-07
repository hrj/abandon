package co.uproot.abandon

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import com.typesafe.config.{ConfigException, ConfigFactory}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ConfigTest extends AnyFlatSpec with Matchers with OptionValues {
  private def write(path: Path, contents: String): Path = {
    Files.write(path, contents.getBytes(StandardCharsets.UTF_8))
    path
  }

  private def minimalConfig(inputs: Seq[String]): String = {
    val renderedInputs = inputs.map(input => "\"" + input.replace("\\", "\\\\") + "\"").mkString(", ")
    s"""
       |inputs = [$renderedInputs]
       |reports = [{ title = "Balances", type = "balance" }]
       |""".stripMargin
  }

  private def cli(arguments: Seq[String]): AbandonCLIConf = {
    val config = new AbandonCLIConf(arguments)
    config.verify()
    config
  }

  "getCompleteSettings" should "use safe defaults without a configuration file" in {
    val settings = SettingsHelper.getCompleteSettings(cli(Seq.empty), "1.2.3").toOption.get

    settings.inputs shouldBe empty
    settings.constraints shouldBe empty
    settings.accounts shouldBe empty
    settings.exports shouldBe empty
    settings.reports shouldBe Seq(BalanceReportSettings("All Balances", None, Nil, true))
    settings.reportOptions shouldBe ReportOptions(Nil)
    settings.configFileOpt shouldBe None
    settings.quiet shouldBe false
    settings.version shouldBe Some(VersionId("1.2.3"))
    settings.txnFilters shouldBe None
  }

  it should "honor command-line defaults and filter overrides" in {
    val settings = SettingsHelper.getCompleteSettings(
      cli(Seq("-i", "one.abandon", "two.abandon", "-q", "-X", "--filter", "payee=Shop")),
      "1.2.3"
    ).toOption.get

    settings.inputs shouldBe Seq("one.abandon", "two.abandon")
    settings.quiet shouldBe true
    settings.version shouldBe None
    settings.txnFilters.value shouldBe ANDTxnFilterStack(Seq(PayeeTxnFilter("Shop")))
  }

  "createTxnFilter" should "construct every supported filter" in {
    SettingsHelper.createTxnFilter("onOrAfter", "2024-02-29") shouldBe OnOrAfterDateTxnFilter(Date(2024, 2, 29))
    SettingsHelper.createTxnFilter("before", "2024-03-01") shouldBe BeforeDateTxnFilter(Date(2024, 3, 1))
    SettingsHelper.createTxnFilter("payee", "Shop") shouldBe PayeeTxnFilter("Shop")
    SettingsHelper.createTxnFilter("account", "Assets") shouldBe AccountNameTxnFilter("Assets")
    SettingsHelper.createTxnFilter("annotation", "tax") shouldBe AnnotationTxnFilter("tax")
  }

  it should "reject unknown filters and invalid dates" in {
    the[SettingsError] thrownBy SettingsHelper.createTxnFilter("unknown", "value") should have message "Unknown filter: unknown"
    val error = the[SettingsError] thrownBy SettingsHelper.createTxnFilter("before", "not-a-date")
    error.getMessage should startWith("Filters\n   Invalid date: not-a-date")
  }

  "makeSettings" should "report a missing configuration file" in {
    val missing = Files.createTempDirectory("abandon-config").resolve("missing.conf")

    val result = SettingsHelper.makeSettings(missing.toString, Some(VersionId("1")), quiet = false, None)

    result.left.toOption.value should include(s"Config file not found: $missing")
  }

  it should "return configuration parse and validation errors" in {
    val malformed = write(Files.createTempFile("abandon-malformed", ".conf"), "inputs = [")
    val invalidReport = write(
      Files.createTempFile("abandon-invalid-report", ".conf"),
      """inputs = []
        |reports = [{ title = "Bad", type = "unknown" }]
        |""".stripMargin
    )

    SettingsHelper.makeSettings(malformed.toString, None, quiet = false, None).isLeft shouldBe true
    SettingsHelper.makeSettings(invalidReport.toString, None, quiet = false, None).left.toOption.value should
      include("expected 'balance', 'register' or 'book'")
  }

  it should "return user-facing errors for invalid filters and constraints" in {
    val malformedFilter = write(
      Files.createTempFile("abandon-invalid-filter", ".conf"),
      minimalConfig(Nil) + """filters = ["missing-separator"]"""
    )
    val unknownFilter = write(
      Files.createTempFile("abandon-unknown-filter", ".conf"),
      minimalConfig(Nil) + """filters = ["unknown=value"]"""
    )
    val unknownConstraint = write(
      Files.createTempFile("abandon-unknown-constraint", ".conf"),
      minimalConfig(Nil) + """eodConstraints = [{ expr = "Assets", constraint = "zero" }]"""
    )

    SettingsHelper.makeSettings(malformedFilter.toString, None, quiet = false, None).left.toOption.value should
      include("expected name=value")
    SettingsHelper.makeSettings(unknownFilter.toString, None, quiet = false, None).left.toOption.value shouldBe
      "Unknown filter: unknown"
    SettingsHelper.makeSettings(unknownConstraint.toString, None, quiet = false, None).left.toOption.value should
      include("expected 'positive', 'negative' or 'equals'")
  }

  it should "expand plain, glob, and regex inputs relative to the configuration file" in {
    val dir = Files.createTempDirectory("abandon-inputs")
    val nested = Files.createDirectories(dir.resolve("nested"))
    val plain = write(dir.resolve("plain.abandon"), "")
    val globbed = write(nested.resolve("globbed.abandon"), "")
    val regexed = write(nested.resolve("regexed.ledger"), "")
    write(nested.resolve("ignored.txt"), "")
    val config = write(
      dir.resolve("accounts.conf"),
      minimalConfig(Seq("plain.abandon", "glob:nested/*.abandon", "regex:nested/.*\\.ledger"))
    )

    val settings = SettingsHelper.makeSettings(config.toString, None, quiet = false, None).toOption.get

    settings.inputs shouldBe Seq(plain, globbed, regexed).map(_.toFile.getCanonicalPath).sorted
    settings.txnFilters shouldBe None
  }

  it should "load configuration filters unless command-line filters take precedence" in {
    val config = write(
      Files.createTempFile("abandon-filters", ".conf"),
      minimalConfig(Nil) + """filters = ["account=Assets", "annotation=tax"]"""
    )
    val cliFilters = ANDTxnFilterStack(Seq(PayeeTxnFilter("CLI")))

    val configured = SettingsHelper.makeSettings(config.toString, None, quiet = false, None).toOption.get
    val overridden = SettingsHelper.makeSettings(config.toString, None, quiet = false, Some(cliFilters)).toOption.get

    configured.txnFilters.value shouldBe ANDTxnFilterStack(
      Seq(AccountNameTxnFilter("Assets"), AnnotationTxnFilter("tax"))
    )
    overridden.txnFilters shouldBe Some(cliFilters)
  }

  "report and export settings" should "apply optional defaults" in {
    val balance = SettingsHelper.makeReportSettings(
      ConfigFactory.parseString("""title = "Balances", type = "balance"""")
    )
    val register = SettingsHelper.makeReportSettings(
      ConfigFactory.parseString("""title = "Register", type = "register"""")
    )
    val exportSettings = SettingsHelper.makeExportSettings(
      ConfigFactory.parseString("""type = "balance", format = "ledger""""),
      Some(VersionId("1"))
    )

    balance shouldBe BalanceReportSettings("Balances", None, Nil, false)
    register shouldBe RegisterReportSettings("Register", None, Nil, GroupByMonth())
    exportSettings shouldBe BalanceExportSettings(LedgerType, None, Nil, Some(VersionId("1")), false, Nil)
  }

  it should "reject invalid report, export type, and export format values" in {
    assertThrows[ConfigException.BadValue] {
      SettingsHelper.makeReportSettings(ConfigFactory.parseString("""title = "Bad", type = "other""""))
    }
    assertThrows[ConfigException.BadValue] {
      SettingsHelper.makeExportSettings(ConfigFactory.parseString("""type = "other", format = "ledger""""), None)
    }
    assertThrows[ConfigException.BadValue] {
      SettingsHelper.makeExportSettings(ConfigFactory.parseString("""type = "balance", format = "csv""""), None)
    }
  }
}