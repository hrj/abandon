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
import Helper.{ Zero, maxElseZero, sumDeltas, minElseZero }

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
          selectedItems.collect{
            case BalanceReportEntry(Some(acc), render) => BarChart.aChart(acc)
          }
        }
      }

      object BarChart extends JFXApp {
        def aChart(acc: AccountName) = {
          val sortedGroup = appState.accState.txnGroups.sortBy(_.date.toInt)
          var years = Seq[String]()
          var acc1 = Seq[Int]()
          if (sortedGroup.isEmpty) {
            Nil
          } else {
            val accAmounts = appState.accState.amounts.toSeq
            val maxAmount = maxElseZero(accAmounts.map(_._2.toInt))
            val minAmount = minElseZero(accAmounts.map(_._2.toInt))
            sortedGroup.map { a =>
              a.children.map { b =>
                if (b.name.equals(acc)) {
                  acc1 :+= b.delta.toInt
                  years :+= a.date.formatCompact
                }
              }
            }
            val xAxis = CategoryAxis(ObservableBuffer(years))
            val yAxis = NumberAxis(
              axisLabel = "Amounts",
              lowerBound = minAmount,
              upperBound = maxAmount,
              tickUnit = 20000
            )
            stage = new JFXApp.PrimaryStage {
              title = "Bar Chart"
              scene = new Scene(500, 500) {
                root = new BarChart(xAxis, yAxis) {
                  title = "Bar Chart"
                  categoryGap = 25
                  data = ObservableBuffer(
                    xySeries("Region 1", acc1)
                  )
                }
              }
            }
          }
          /** Create XYChart.Series from a sequence of numbers matching year strings. */
          def xySeries(name: String, data: Seq[Int]) = {
            val series = years zip data
            XYChart.Series[String, Number](
              name,
              ObservableBuffer(series.map { case (x, y) => XYChart.Data[String, Number](x, y) })
            )
          }
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
