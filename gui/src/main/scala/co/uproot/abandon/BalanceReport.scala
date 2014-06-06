package co.uproot.abandon

import scalafx.Includes._
import scalafx.scene.layout.HBox
import scalafx.scene.control.ListView
import scalafx.scene.layout.Priority
import scalafx.scene.input.KeyEvent
import scalafx.scene.control.ListCell
import javafx.scene.input.MouseEvent
import javafx.event.EventHandler
import scalafx.scene.chart.{ AreaChart, NumberAxis, XYChart }
import scalafx.collections.ObservableBuffer
import scalafx.scene.chart.Axis
import scalafx.application.JFXApp
import scalafx.collections.ObservableBuffer
import scalafx.scene.Scene
import scalafx.scene.chart._
import Helper._
import scalafx.stage.Stage
import javafx.embed.swing.JFXPanel

object BalanceReport extends Report {

  def mkBalanceReport(appState: AppState, settings: Settings, reportSettings: BalanceReportSettings) = {
    val (leftRender, rightRender, totalLeft, totalRight) = Reports.balanceReport(appState, settings, reportSettings)
    val padLength = Math.max(leftRender.length, rightRender.length) + 1
    val left = leftRender.padTo(padLength, BalanceReportEntry(None, "")) :+ BalanceReportEntry(None, totalLeft)
    val right = rightRender.padTo(padLength, BalanceReportEntry(None, "")) :+ BalanceReportEntry(None, totalRight)

    class BalanceView(entries: Seq[BalanceReportEntry]) extends ListView(entries) {
      hgrow = Priority.ALWAYS
      onMouseClicked = new EventHandler[MouseEvent] {
        override def handle(even: MouseEvent) {
          val selectedItems = selectionModel().getSelectedItems()
          selectedItems.foreach { a =>
            a match {
              case BalanceReportEntry(Some(acc), render) =>
                BarChart.aChart(acc)
              case _ =>
            }
          }
        }
      }

      object BarChart {
        def xySeries(name: String, data: Seq[Double], dates: Seq[String]) = {
          val series = dates zip data
          XYChart.Series[String, Number](
            name,
            ObservableBuffer(series.map { case (x, y) => XYChart.Data[String, Number](x, y) })
          )
        }

        def aChart(accName: AccountName) = {
          val reportGroups = appState.accState.txnGroups
          val accountsGroups = reportGroups.flatMap(_.children).filter(_.name equals accName).groupBy(_.date).toSeq
          val accounts = accountsGroups.map{ acc =>
            (acc._2.last.delta, acc._1)
          }
          val amounts = accounts.map(_._1.toDouble)

          val maxAmount = maxElseZero(amounts)
          val minAmount = minElseZero(amounts)
          val (adjustedMaxAmount, adjustedMinAmount) =
            if (maxAmount equals minAmount) {
              if (maxAmount > 0) (maxAmount, 0.0) else (0.0, minAmount)
            } else if (maxAmount < 0 && minAmount < 0) (0.0, minAmount)
            else if (maxAmount > 0 && minAmount > 0) (maxAmount, 0.0)
            else (maxAmount, minAmount)

          val accAmount = accounts.map(_._1.toDouble)
          val dates = accounts.map(_._2.formatCompact)

          val xAxis = CategoryAxis(dates)
          val yAxis = NumberAxis(
            axisLabel = "Amounts",
            lowerBound = adjustedMinAmount,
            upperBound = adjustedMaxAmount,
            tickUnit = 1000
          )
          val dialogStage = new Stage {
            title = accName.fullPathStr
            scene = new Scene(1200, 1200) {
              root = new BarChart(xAxis, yAxis) {
                title = "Balance v/s Time"
                categoryGap = 25
                data = ObservableBuffer(
                  xySeries("Region 1", accAmount, dates)
                )
              }
            }
          }
          dialogStage.show
        }
      }

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
              Some(selectedAccountPatterns),
              Nil
            )
          CurrReports.addReport(appState, settings, regSettings, canClose = true)
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
      styleClass += styleClassName
      content = Seq(
        new BalanceView(left),
        new BalanceView(right)
      )
    }
  }

}
