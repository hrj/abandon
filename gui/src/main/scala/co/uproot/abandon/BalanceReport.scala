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
            case BalanceReportEntry(Some(acc), render) =>
              BarChart.aChart(acc)
          }
        }
      }

      object BarChart extends JFXApp {
        def aChart(accName1: AccountName) = {
          var years = Seq[String]()
          var acc1 = Seq[Int]()
          var accTreeState = Seq[AccountTreeState]()
          var accName = Seq[AccountName]()
          var flag2 = 0
          def isRenderable(accTree: AccountTreeState): Boolean = {
            if (accTree.countRenderableChildren(accTree => true) != 0) {
              true
            } else {
              false
            }

          }
          def trace(accTreeState1: AccountTreeState): Seq[AccountName] = {
            accTreeState1.childStates.map { c =>
              if (flag2 == 0) {
                if (c.name.fullPathStr.matches("""^""" + accName1.fullPathStr + """.*""") == true) {
                  if (isRenderable(c)) {
                    if (c.amount.toInt >= 0) {
                      acc1 :+= c.amount.toInt
                      accName :+= c.name
                    }
                    trace(c)
                  } else {
                    acc1 :+= c.total.toInt
                    accName :+= c.name

                  }
                } else {
              if (isRenderable(c)) {
                trace(c)
              }
                }
              }
              if (c.name.fullPathStr.equals(accName1.fullPathStr) && isRenderable(c) == false) {
                acc1 = Seq(c.total.toInt)
                accName = Seq(c.name)
                flag2 = 1
              }
            }
            accName
          }
          def childCheck(z: AccountTreeState, acc: Seq[AccountName]): Seq[Int] = {
            accTreeState = Seq(z)
            if (z.name.fullPathStr.equals(accName1.fullPathStr)) {
              if (z.amount.toInt != 0) {
                acc1 :+= z.amount.toInt
                accName :+= z.name
                accTreeState = Seq(z)
              }
            }
            accTreeState.map { A =>
              if (accName1.fullPathStr.matches("""^""" + A.name.fullPathStr + """.*""") == true) {
                A.childStates.map { c =>
                  if (c.name.fullPathStr.equals(accName1.fullPathStr) && isRenderable(c) == false) {
                    acc1 = Seq(c.total.toInt)
                    accName = Seq(c.name)
                    flag2 = 1
                  }
                  if (flag2 == 0) {
                    if (isRenderable(c)) { 
                      if (c.amount.toInt != 0) {
                        if(c.name.fullPathStr.equals(accName1.fullPathStr) || c.name.fullPathStr.matches("""^""" + accName1.fullPathStr + """.*""")) {
                        acc1 :+= c.amount.toInt
                        accName :+= c.name
                        }
                      }
                      trace(c)
                    } else {
                    if(c.name.fullPathStr.equals(accName1.fullPathStr) || c.name.fullPathStr.matches("""^""" + accName1.fullPathStr + """.*""")) {
                      acc1 :+= c.total.toInt
                      accName :+= c.name
                    }
                    }
                  }
                }
              }
            }
            acc1
          }
          val sortedGroup = appState.accState.txnGroups.sortBy(_.date.toInt)
          val txnGroups = appState.accState.mkTree(_.exists(c => reportSettings.isAccountMatching(accName1.fullPathStr)))
          txnGroups.childStates.map { a =>
            childCheck(a, accName)
          }
          if (sortedGroup.isEmpty) {
            Nil
          } else {
            val accAmounts = appState.accState.amounts.toSeq
            val maxAmount = maxElseZero(accAmounts.map(_._2.toInt))
            val minAmount = minElseZero(accAmounts.map(_._2.toInt))
            var flag = 0
            sortedGroup.map { a =>
              a.children.map { b =>
                accName.map { B =>
                  if (B.fullPathStr.equals(b.name.fullPathStr) && flag == 0) {
                    years :+= b.date.formatCompact
                    flag = flag + 1
                  } else {
                    if (B.fullPathStr.equals(b.name.fullPathStr) && flag != 0) {
                      years = (flag + 1).toString +: years
                      flag = flag + 2
                    }
                  }

                }
              }
            }
            //   val xAxis = CategoryAxis(ObservableBuffer(years))
            val xAxis = CategoryAxis(years)
            val yAxis = NumberAxis(
              axisLabel = "Amounts",
              lowerBound = minAmount - 1000,
              upperBound = maxAmount,
              tickUnit = 1000
            )
            stage = new JFXApp.PrimaryStage {
              title = "Bar Chart"
              scene = new Scene(1200, 1200) {
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
