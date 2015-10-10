package ch2
package org.learningconcurrency
/**
 * Created by JC on 2015/10/9.
 */
object SynchronizedNesting extends App{
  import scala.collection._
  private val transfers = mutable.ArrayBuffer[String]()
  def thread(body: => Unit): Thread = {
    val t = new Thread{
      override def run() = body
    }
    t.start()
    t
  }
  def logTransfer(name: String, n: Int) = transfers.synchronized{
    transfers += s"transfer to account '$name' = $n"
  }
  class Account(val name: String, var money: Int)
  def add(account: Account, n: Int) = account.synchronized{
    account.money += n
    if(n > 10) logTransfer(account.name, n)
  }
  /////////////////////////
  val jane = new Account("Jane", 100)
  val john = new Account("John", 200)
  val t1 = thread{  add(jane, 5)  }
  val t2 = thread{  add(john, 50) }
  val t3 = thread{  add(jane, 70) }

  t1.join()
  t2.join()
  t3.join()
  log(s"--- transfers ---\n$transfers")
}

object SynchronizedDeadlock extends App{
  import SynchronizedNesting.Account
  def send(a: Account, b: Account, n:Int) = a.synchronized{
    b.synchronized{
      a.money -= n
      b.money += n
    }
  }
}

object SynchronizedBadPool extends App{
  import scala.collection._
  private val tasks = mutable.Queue[()=>Unit]()   // to store the scheduled blocks of code
  val worker = new Thread {
      def poll(): Option[() => Unit] = tasks.synchronized{
        if (tasks.nonEmpty) Some(tasks.dequeue()) else None
      }
      override def run() = while (true) poll()  match {
        case Some(task) => task()
        case None =>
      }
    }
  worker.setName("Worker")
  worker.setDaemon(true)
  worker.start()

  def asynchronous(body: => Unit) = tasks.synchronized{
    tasks.enqueue(()=>body)
  }
  asynchronous{log("Hello")}
  asynchronous{log(" World!")}
  Thread.sleep(5000)
}

object SynchronizedGuardedBlocks extends App{
  val lock = new AnyRef
  var message: Option[String] = None
  import SynchronizedNesting.thread
  val greeter = thread{
    lock.synchronized{
      while (message == None) lock.wait()   // release lock
      log(message.get)
    }
  }
  lock.synchronized{
    message = Some("Hello!")
    lock.notify()   // release lock
  }
  greeter.join()
}

object SynchronizedPool extends App {
  import collection._
  private val tasks = mutable.Queue[()=>Unit]()
  object Worker extends Thread {
    setDaemon(true)
    def poll() = tasks.synchronized{
      while(tasks.isEmpty)  tasks.wait()
      tasks.dequeue()
    }
    override def run() =while(true) {
      val task = poll()
      task()
    }
  }
  Worker.start()
  def asynchronous(body: => Unit) = tasks.synchronized{
    tasks.enqueue(()=>body)
    tasks.notify()
  }

  asynchronous{log("Hello")}
  asynchronous{log("World!")}
}