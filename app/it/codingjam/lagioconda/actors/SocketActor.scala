package it.codingjam.lagioconda.actors

import akka.actor.{Props, Actor, ActorRef}
import it.codingjam.lagioconda.protocol.InEvent
import it.codingjam.lagioconda.protocol.Messages.{Start, Individual}
import it.codingjam.lagioconda.services.ImageGenerator
import scala.concurrent.duration._

class SocketActor(out: ActorRef, imageGenerator: ImageGenerator)
    extends Actor {

  implicit val executor = context.system.dispatcher
  context.system.scheduler.schedule(1.seconds, 2.seconds, self, Start(0))

  def receive = {
    case msg: InEvent =>
      out ! Individual(0, imageGenerator.create(400, 400))
  }
}

object SocketActor {
  def props(out: ActorRef, ig: ImageGenerator) =
    Props(new SocketActor(out, ig))
}
