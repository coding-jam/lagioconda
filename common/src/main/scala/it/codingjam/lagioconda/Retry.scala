package it.codingjam.lagioconda

import akka.actor.Scheduler
import akka.pattern.after

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._

trait Retrying {
  def retry[T](f: => Future[T], delay: FiniteDuration, retries: Int)(implicit ec: ExecutionContext, s: Scheduler): Future[T] = {
    f recoverWith { case _ if retries > 0 => after(delay, s)(retry(f, delay, retries - 1)) }
  }
}
