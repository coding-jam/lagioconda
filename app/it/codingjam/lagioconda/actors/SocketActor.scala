package it.codingjam.lagioconda.actors

import akka.actor.{Props, Actor, ActorRef}
import it.codingjam.lagioconda.actors.PopulationActor.{
  MigrationDone,
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

  var populationActors: List[ActorRef] = List()

  self ! Start(0)

  def receive = {
    case msg: InEvent =>
      println("CMD" + msg)
      if (populationActors.isEmpty) {
        Range(0, 4).foreach { i =>
          val a =
            context.actorOf(PopulationActor
                              .props(out)
                              .withDispatcher("population-actor-dispatcher"),
                            name = "pop" + i)
          populationActors = populationActors :+ a
          a ! SetupPopulation(i)
        }
      }

    case msg: GenerationRan =>
      if (Random.nextInt(100) < 5) {
        val r = Random.nextInt(populationActors.size)
        val destination =
          if (r == msg.index)
            (r + 1) % populationActors.size
          else
            r
        populationActors(msg.index) ! PopulationActor.Migrate(
          msg.index,
          50,
          populationActors(destination))
      } else
        populationActors(msg.index) ! PopulationActor.RunAGeneration(msg.index)

    case msg: PopulationGenerated =>
      populationActors(msg.index) ! PopulationActor.RunAGeneration(msg.index)

    case msg: MigrationDone =>
      populationActors(msg.index) ! PopulationActor.RunAGeneration(msg.index)
  }
}

object SocketActor {
  def props(out: ActorRef, ig: ImageGenerator) =
    Props(new SocketActor(out, ig))

  case object GenerateAction

  case class PopulationGenerated(index: Int)

  case class GenerationRan(index: Int)
}
