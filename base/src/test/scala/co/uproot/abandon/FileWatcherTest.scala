package co.uproot.abandon

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.File
import java.nio.file.Files

class FileWatcherTest extends AnyFlatSpec with Matchers {
  "FileWatcher" should "continue watching and polling even if onChange throws an exception" in {
    val tempFile = Files.createTempFile("abandon_watcher_test", ".txt").toFile
    tempFile.deleteOnExit()

    val watcher = new FileWatcher(pollDelay = 100, triggerDelay = 50)
    var callCount = 0

    watcher.watch(Set(tempFile.getAbsolutePath), () => {
      callCount += 1
      if (callCount == 1) {
        throw new RuntimeException("Simulated exception in onChange")
      }
      None
    })

    // Modify file to trigger watcher first time
    Thread.sleep(200)
    Files.write(tempFile.toPath, "change 1".getBytes)

    // Wait for watcher to trigger first time (which throws exception)
    var waits = 0
    while (callCount < 1 && waits < 30) {
      Thread.sleep(100)
      waits += 1
    }
    callCount should be(1)

    // Modify file to trigger watcher second time
    Thread.sleep(200)
    Files.write(tempFile.toPath, "change 2".getBytes)

    // Wait for watcher to trigger second time (proving thread is still alive)
    waits = 0
    while (callCount < 2 && waits < 30) {
      Thread.sleep(100)
      waits += 1
    }

    watcher.stopWatch
    callCount should be(2)
  }
}
