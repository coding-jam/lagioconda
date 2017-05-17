package it.codingjam.lagioconda

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import akka.pattern.after
import akka.actor.Scheduler

trait Retrying {
  def retry[T](f: => Future[T], delay: FiniteDuration, retries: Int)(implicit ec: ExecutionContext, s: Scheduler): Future[T] = {
    f recoverWith { case _ if retries > 0 => after(delay, s)(retry(f, delay, retries - 1)) }
  }
}
