package it.codingjam.lagioconda.actors

import akka.actor.{Props, Actor, ActorRef}
import it.codingjam.lagioconda.protocol.InEvent
import it.codingjam.lagioconda.protocol.Messages.Individual
import it.codingjam.lagioconda.services.ImageGenerator

class SocketActor(out: ActorRef, imageGenerator: ImageGenerator)
    extends Actor {
  def receive = {
    case msg: InEvent =>
      out ! Individual(0, imageGenerator.create(400, 400))
  }
}

object SocketActor {
  def props(out: ActorRef, ig: ImageGenerator) =
    Props(new SocketActor(out, ig))
}
