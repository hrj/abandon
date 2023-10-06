package co.uproot.abandon

import java.util.{Timer, TimerTask}

class FileWatcher(pollDelay: Long = 1000, triggerDelay: Long = 500) {
  case class FileProperties(lastModified: Long, size: Long)
  private var propertyCache = Map[String, FileProperties]()

  var paths = Set[String]()
  def watch(pathsToWatch: Set[String], onChangeToCall: () => Option[Set[String]]) = {
    timer.cancel
    timer = mkTimer
    onChange = onChangeToCall
    paths = pathsToWatch
    paths.foreach { path =>
      val properties = getProperties(path)
      propertyCache += (path -> properties)
    }
    runCheckTask
  }

  private var onChange: () => Option[Set[String]] = _

  def stopWatch = {
    timer.cancel()
  }

  private def mkTask = new TimerTask {
    def run = checkAndUpdate
  }
  private def runCheckTask: Unit = {
    timer.schedule(mkTask, pollDelay)
  }

  private def checkAndUpdate = {
    var change = false
    paths.foreach { path =>
      val properties = getProperties(path)
      propertyCache.get(path) match {
        case Some(oldProperties) if (oldProperties equals properties) =>
        case _ =>
          propertyCache += (path -> properties)
          change = true
      }
    }
    if (change) {
      println("Change detected")
      // Sleep to avoid catching files in the middle of modifications
      Thread.sleep(triggerDelay)
      println("Triggering")
      onChange().foreach(paths = _)
    }
    runCheckTask
  }

  private def getProperties(path: String) = {
    val f = new java.io.File(path)
    FileProperties(f.lastModified(), f.length)
  }

  private var timer = mkTimer
  private def mkTimer = new Timer("file watcher")
}
