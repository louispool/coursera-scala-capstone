package observatory

import java.util.concurrent.{ForkJoinPool, ForkJoinTask, ForkJoinWorkerThread, RecursiveTask}

import scala.util.DynamicVariable

package object scheduler {

  val forkJoinPool = new ForkJoinPool

  class TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T] = {
      val task = new RecursiveTask[T] {
        def compute = body
      }

      Thread.currentThread match {
        case wt: ForkJoinWorkerThread =>
          task.fork()
        case _ =>
          forkJoinPool.execute(task)
      }
      task
    }
  }
  val scheduler = new DynamicVariable[TaskScheduler](new TaskScheduler)

  def task[T](body: => T): ForkJoinTask[T] = scheduler.value.schedule(body)

  def parallel[A, B, C, D](taskA: => A, taskB: => B, taskC: => C, taskD: => D): (A, B, C, D) = {
    val ta = task(taskA)
    val tb = task(taskB)
    val tc = task(taskC)
    val td = taskD

    (ta.join(), tb.join(), tc.join(), td)
  }
}
