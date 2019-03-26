//import java.util.concurrent._
//
//class CountingSemaphore private(limit: Int, scheduler: ScheduledExecutorService = Executors.newScheduledThreadPool(1)) {
//  private var signals = 0
//
//  def take(): Unit = this.synchronized {
//    while (this.signals == limit) wait()
//    this.signals += 1
//  }
//
//  def getRemainingCount: Int = limit - signals
//
//  private def reset(): Unit = this.synchronized {
//    this.signals = 0
//    this.notifyAll()
//  }
//
//  private def schedule(initialDelay: Int, fixedDelay: Int, period: TimeUnit): Unit = {
//    scheduler.scheduleAtFixedRate(() => {
//      println("Hello")
//      this.reset()
//    }, initialDelay, fixedDelay, period)
//  }
//}
//
//object CountingSemaphore {
//  def apply(limit: Int, initialDelay: Int, fixedDelay: Int, period: TimeUnit): CountingSemaphore = {
//    val cs = new CountingSemaphore(limit)
//    cs.schedule(initialDelay, fixedDelay, period)
//    cs
//  }
//}
//
//val cs = CountingSemaphore(5, 0, 5, TimeUnit.SECONDS)
//
//cs.take()
//
//cs.getRemainingCount
//
//cs.take()
//
//cs.take()
//cs.take()
//cs.take()
//
//cs.take()
//cs.take()
//
//cs.getRemainingCount
//
//Thread.sleep(3000)
//
//cs.take()
//
//cs.getRemainingCount
//
//Thread.sleep(3000)
//
//cs.take()
//
//cs.getRemainingCount
//
//cs.take()
//
//cs.getRemainingCount
//
//cs.take()
//
//cs.getRemainingCount
//
//println("HELLO!")