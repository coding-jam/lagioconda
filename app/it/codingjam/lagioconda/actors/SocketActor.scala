package it.codingjam.lagioconda.actors

import akka.actor.{Props, Actor, ActorRef}
import it.codingjam.lagioconda.actors.PopulationActor.{
  Mutate,
  RemoveWeaker,
  SetupPopulation,
  Crossover
}
import it.codingjam.lagioconda.actors.SocketActor.{
  GenerationRan,
  PopulationGenerated,
  GenerateAction
}
import it.codingjam.lagioconda.protocol.InEvent
import it.codingjam.lagioconda.protocol.Messages.{Start, Individual}
import it.codingjam.lagioconda.services.ImageGenerator
import scala.concurrent.duration._
import scala.util.Random

class SocketActor(out: ActorRef, imageGenerator: ImageGenerator)
    extends Actor {

  implicit val executor = context.system.dispatcher

  var masterActor: Option[ActorRef] = None

  self ! Start(0)

  def receive = {
    case msg: InEvent =>
      println("CMD" + msg)
      if (masterActor.isEmpty) {
        masterActor = Some(
          context.actorOf(PopulationActor.props(out), name = "master"))
        masterActor.foreach(a => a ! SetupPopulation)

      }

    case GenerationRan =>
      masterActor.foreach(a => a ! PopulationActor.RunAGeneration)

    case PopulationGenerated =>
      masterActor.foreach(a => a ! PopulationActor.RunAGeneration)
  }
}

object SocketActor {
  def props(out: ActorRef, ig: ImageGenerator) =
    Props(new SocketActor(out, ig))

  case object GenerateAction

  case object PopulationGenerated

  case object GenerationRan
}
