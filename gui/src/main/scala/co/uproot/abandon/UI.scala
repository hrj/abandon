package co.uproot.abandon

import javafx.event.EventHandler
import javafx.stage.WindowEvent

import org.rogach.scallop.exceptions.{Help, Version}

import scala.collection.JavaConverters._
import scalafx.Includes._
import scalafx.application.JFXApp.PrimaryStage
import scalafx.application.{JFXApp, Platform}
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.{Label, Tab, TabPane}
import scalafx.scene.layout.{BorderPane, HBox}
import scalafx.scene.{Node, Scene}

trait UIReport {
  protected val styleClassName = "report"
}

object CurrReports {
  private var reportSettingsCache = Seq[ReportSettings]()

  def addReport(appState: AppState, settings: Settings, rs: ReportSettings, canClose: Boolean = false): Unit = {
    reportSettingsCache :+= rs
    showReport(appState, settings, rs, canClose)
  }

  def updateAll(appState: AppState, settings: Settings) = {
    reportSettingsCache.foreach { rs =>
      showReport(appState, settings, rs, false)
    }
  }

  def showReport(appState: AppState, settings: Settings, rs: ReportSettings, canClose: Boolean) = {
    val reportRender = rs match {
      case regSettings: RegisterReportSettings =>
        RegUIReport.mkRegisterReport(appState, regSettings)
      case balSettings: BalanceReportSettings =>
        BalanceUIReport.mkBalanceReport(appState, settings, balSettings)
      case bookSettings: BookReportSettings =>
        // TODO
        RegUIReport.mkRegisterReport(appState, RegisterReportSettings(bookSettings.title, bookSettings.accountMatch, Nil, GroupByMonth()))
    }
    AbandonUI.tabPane.addOrSetTab(rs.title, reportRender, canClose)
  }

}

object AbandonUI extends JFXApp {

  private val infoTab =
    new Tab {
      text = "Info"
      closable = false
      content = new Label("Please Wait: Processing input files") {
        alignmentInParent = Pos.Center
        style = "-fx-font:16 Sans;"
      }
    }

  private def updateInfo(appState: AppState, settings: Settings, processedFiles: Set[String]) = {
    val warnings = FilterStackHelper.getFilterWarnings(settings.txnFilters, "   ")
    val warningsTxt =
      if (warnings.isEmpty) {
        ""
      } else {
        "ACTIVE FILTER\n" + warnings.mkString("\n") + "\n\n"
      }

    infoTab.content = new Label(warningsTxt + "Processed files:\n" + processedFiles.mkString("\n"))
  }

  val tabPane = new TabPane {
    def addTab(t: Tab): Unit = {
      this += t
    }
    def addTab(title: String, node: Node, canClose: Boolean = false): Unit =
      addTab(new Tab {
        text = title
        content = node
        closable = canClose
      })
    def addOrSetTab(tab: Tab): Unit = {
      val existingOpt = tabs.iterator.asScala.find(_.getText equals tab.getText)
      existingOpt match {
        case Some(existingTab) =>
          existingTab.setContent(tab.getContent)
        case None =>
          this += tab
      }
    }
    def addOrSetTab(title: String, node: Node, canClose: Boolean = false): Unit = {
      addOrSetTab(new Tab {
        text = title
        content = node
        closable = canClose
      })
    }

    tabs = Seq(infoTab)
  }

  private object StatusBar extends HBox {
    style = "-fx-font:14 Sans; -fx-background-color:#ddd"
    val label = new Label("Ready")
    padding = Insets(10, 10, 10, 10)
    children = Seq(
      label
    )

    def setText(txt: String) = {
      label.setText(txt)
    }
  }

  private val mainPane =
    new BorderPane {
      center = tabPane
      bottom = StatusBar
    }

  stage = new PrimaryStage {
    title = "Abandon"
    scene = new Scene(1000, 600) {
      root = mainPane
      stylesheets += "default_theme.css"
    }
    onCloseRequest = new EventHandler[WindowEvent] {
      def handle(e: WindowEvent) = {
        inputFileWatcher.stopWatch
      }
    }
  }

  private val inputFileWatcher = new FileWatcher

  private def createReportTabs(firstRun: Boolean, settings: Settings) = {
    val (parseError, astEntries, processedFiles) = Processor.parseAll(settings.inputs, settings.quiet)
    if (!parseError) {
      val appState = Processor.process(astEntries, settings.accounts, settings.txnFilters)
      if (firstRun) {
        settings.reports.foreach(CurrReports.addReport(appState, settings, _))
      } else {
        CurrReports.updateAll(appState, settings)
      }
      updateInfo(appState, settings, processedFiles)
    } else {
      // TODO Show error
    }
    processedFiles
  }

  private def buildId: String = {
    "Base: " + BaseBuildInfo.version + " [" + BaseBuildInfo.builtAtString + "];" +
      "GUI: " +  GuiBuildInfo.version + " [" + GuiBuildInfo.builtAtString + "];"
  }

  try {
    val cliConf = new AbandonCLIConf(parameters.raw)
    cliConf.version("Version: " + GuiBuildInfo.version + " [" + GuiBuildInfo.builtAtString + "]")
    cliConf.verify()

    val settingsResult = SettingsHelper.getCompleteSettings(cliConf, buildId)
    settingsResult match {
      case Left(errorMsg) => handleError("Error: " + errorMsg)
      case Right(settings) =>

        def updateReports(firstRun: Boolean): Unit = {
          val processedFiles = createReportTabs(firstRun, settings)
          Platform.runLater {
            val statusBarTxt = (settings.txnFilters match {
              case Some(txnfs) =>  "ACTIVE FILTER! "
              case None => ""
            }) + "Report generated on " + new java.util.Date

            StatusBar.setText(statusBarTxt)
          }
          inputFileWatcher.watch(processedFiles, () => {
            Platform.runLater({ updateReports(false) })
          })
        }

        updateReports(true)

    }
  } catch {
    case Help(_)                =>
    case Version                =>
    case a: AssertionError      => handleError("Error: " + a.getMessage)
    case i: InputError          => handleError("Input error: " + i.getMessage)
    case i: ConstraintError     => handleError("Constraint Failed: " + i.getMessage)
    case e: NotImplementedError => handleError("Some functionality has not yet been implemented. We intend to implement it eventually. More details:\n" + e.getMessage)
    case e: Error               => handleError("Unexpected error: " + e.getMessage)
  }

  private def handleError(msg:String) {
    System.err.println(msg)
    StatusBar.setText(msg) // TODO: Highlight in red
  }
}
