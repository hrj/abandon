package co.uproot.abandon

import scalafx.Includes._
import javafx.event.EventHandler
import javafx.stage.WindowEvent
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.geometry.Insets
import scalafx.scene.{ Node, Scene }
import scalafx.scene.control.{ Label, Tab, TabPane, TreeItem }
import scalafx.scene.layout.{ BorderPane, HBox, Priority }
import scalafx.application.Platform
import collection.JavaConverters._
import scalafx.geometry.Pos

trait Report {
  protected val fontStyle = "-fx-font:15 Monospace;"
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
        RegReport.mkRegisterReport(appState, regSettings)
      case balSettings: BalanceReportSettings =>
        BalanceReport.mkBalanceReport(appState, settings, balSettings)
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
        alignmentInParent = Pos.CENTER
        style = "-fx-font:16 Sans;"
      }
    }

  def updateInfo(appState: AppState, settings: Settings, processedFiles: Set[String]) = {
    infoTab.content = new Label("Processed files:\n" + processedFiles.mkString("\n"))
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

  object StatusBar extends HBox {
    style = "-fx-font:14 Sans; -fx-background-color:#ddd"
    val label = new Label("Ready")
    padding = Insets(10, 10, 10, 10)
    content = Seq(
      label
    )

    def setText(txt: String) = {
      label.setText(txt)
    }
  }

  val mainPane =
    new BorderPane {
      top = new Label("Menu")
      center = tabPane
      // center = new Label("Center") 
      // bottom = new Label("Footer") 
      bottom = StatusBar
    }

  stage = new PrimaryStage {
    title = "Abandon"
    scene = new Scene(1000, 600) {
      root = mainPane
    }
    onCloseRequest = new EventHandler[WindowEvent] {
      def handle(e: WindowEvent) = {
        inputFileWatcher.stopWatch
      }
    }
  }

  val inputFileWatcher = new FileWatcher

  def createReportTabs(firstRun: Boolean, settings: Settings) = {
    val (parseError, astEntries, processedFiles) = Processor.parseAll(settings.inputs)
    if (!parseError) {
      val appState = Processor.process(astEntries)
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

  try {
    val settingsResult = SettingsHelper.getCompleteSettings(parameters.raw)
    settingsResult match {
      case Left(errorMsg) => // TODO
      case Right(settings) =>

        def updateReports(firstRun: Boolean): Unit = {
          val processedFiles = createReportTabs(firstRun, settings)
          Platform.runLater {
            StatusBar.setText("Report generated on " + new java.util.Date)
          }
          inputFileWatcher.watch(processedFiles, () => {
            Platform.runLater({ updateReports(false) })
          })
        }

        updateReports(true)

    }
  } catch {
    case a: AssertionError => println("Error: " + a.getMessage)
    case i: InputError     => println("Input error: " + i.getMessage)
  }
}
