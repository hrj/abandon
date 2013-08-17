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
import scalafx.scene.control.TreeView
import scalafx.scene.control.ListView
import scalafx.application.Platform
import collection.JavaConverters._
import scalafx.geometry.Pos
import scalafx.scene.control.ListCell
import scalafx.scene.input.KeyEvent
import javafx.scene.input.{ KeyCode => jfxKeyCode }
import scalafx.scene.control.TreeCell
import scalafx.stage.Popup
import scalafx.stage.Stage
import scalafx.stage.Modality
import scalafx.scene.layout.VBox
import Helper._
import scalafx.scene.control.ScrollPane

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
        mkRegisterReport(appState, regSettings)
      case balSettings: BalanceReportSettings =>
        mkBalanceReport(appState, settings, balSettings)
    }
    AbandonUI.tabPane.addOrSetTab(rs.title, reportRender, canClose)
  }

  def mkRegisterReport(appState: AppState, reportSettings: RegisterReportSettings) = {
    val registers = Reports.registerReport(appState, reportSettings)
    val registerItems = registers.map { r =>
      new TreeItem(RegisterReportEntry(Nil, r._1)) {
        children = r._2.map(new TreeItem(_))
      }
    }
    val reportRoot = new TreeItem(RegisterReportEntry(Nil, "Register report (account, delta, total)")) {
      children = registerItems
      expanded = true
    }
    new TreeView(reportRoot) {
      style = fontStyle
      onKeyTyped = { e: KeyEvent =>
        // println(e)
        val txStage = new Stage() {
          scene = new Scene(800, 500) {
            root = new ScrollPane {
              content = mkTxnView(selectionModel().getSelectedItems().head.getValue.txns)
            }
          }
          initModality(Modality.APPLICATION_MODAL)
          title = "Transactions"
        }
        txStage.show
      }
      cellFactory = { v =>
        val delegate = new javafx.scene.control.TreeCell[RegisterReportEntry]() {
          override def updateItem(t: RegisterReportEntry, empty: Boolean) = {
            super.updateItem(t, empty)
            setText(if (t != null) t.render else null)
          }
        }
        new TreeCell(delegate)
      }
    }
  }

  private def mkTxnView(txns:Seq[DetailedTransaction]) = {
    new VBox {
      style = fontStyle
      val maxNameLength = maxElseZero(txns.flatMap(_.parent.get.children.map(_.name.fullPathStr.length)))
      content = txns.map(t => new VBox {
        val grp = t.parent.get
        val dateLabel = new Label(s"""${t.date.formatYYYYMMDD} ${grp.payeeOpt.getOrElse("")}""") {style="-fx-font-weight:bold"}
        val grpCommentLabels = grp.groupComments.map(c => new Label("  ;"+c) {style="-fx-font-weight:bold"})
        val childLabels = grp.children.map(c=>
          ("  %-"+maxNameLength+"s %20.2f %s") format (c.name, c.delta, c.commentOpt.map("  ; " + _).getOrElse(""))
        ).map(new Label(_))
        padding = Insets(10, 10, 10, 10)
        content = (dateLabel +: grpCommentLabels) ++ childLabels
      })
    }
  }

  def mkBalanceReport(appState: AppState, settings: Settings, reportSettings: BalanceReportSettings) = {
    val (leftRender, rightRender, totalLeft, totalRight) = Reports.balanceReport(appState, settings, reportSettings)
    val padLength = Math.max(leftRender.length, rightRender.length) + 1
    val left = leftRender.padTo(padLength, BalanceReportEntry(None, "")) :+ BalanceReportEntry(None, totalLeft)
    val right = rightRender.padTo(padLength, BalanceReportEntry(None, "")) :+ BalanceReportEntry(None, totalRight)

    class BalanceView(entries: Seq[BalanceReportEntry]) extends ListView(entries) {
      hgrow = Priority.ALWAYS
      onKeyTyped = { e: KeyEvent =>
        // println ("Typed key", e.character, e.code, jfxKeyCode.ENTER, e.delegate.code)
        // if (e.code equals jfxKeyCode.ENTER) {
        if (e.character equals "\r") {
          val selectedItems = selectionModel().getSelectedItems()
          val selectedAccountNames = selectedItems.flatMap(_.accName)
          val selectedAccountPatterns = selectedAccountNames.map("^" + _.fullPathStr + ".*")
          val regSettings =
            RegisterReportSettings(
              selectedAccountNames.map(_.fullPathStr).mkString(","),
              Some(selectedAccountPatterns)
            )
          addReport(appState, settings, regSettings, canClose = true)
        }
      }
      cellFactory = { v =>
        val delegate = new javafx.scene.control.ListCell[BalanceReportEntry]() {
          override def updateItem(t: BalanceReportEntry, empty: Boolean) = {
            super.updateItem(t, empty)
            setText(if (t != null) t.render else null)
          }
        }
        new ListCell(delegate)
      }
    }

    new HBox {
      def getWidth = width
      hgrow = Priority.ALWAYS
      style = fontStyle
      content = Seq(
        new BalanceView(left),
        new BalanceView(right)
      )
    }
  }

  private val fontStyle = "-fx-font:15 Monospace;"
}

object AbandonUI extends JFXApp {

  private val infoTab =
    new Tab {
      text = "Info"
      closable = false
      content = new Label("TODO: Show info about inputs, etc") {
        alignmentInParent = Pos.CENTER
        style = "-fx-font:16 Sans;"
      }
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
