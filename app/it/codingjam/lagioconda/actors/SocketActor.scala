package it.codingjam.lagioconda.actors

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import it.codingjam.lagioconda.actors.PopulationActor.{MigrationDone, SetupPopulation}
import it.codingjam.lagioconda.actors.SocketActor.{GenerationRan, PopulationGenerated}
import it.codingjam.lagioconda.protocol.InEvent
import it.codingjam.lagioconda.protocol.Messages.Start
import it.codingjam.lagioconda.services.ImageGenerator

import scala.util.Random

class SocketActor(out: ActorRef, imageGenerator: ImageGenerator) extends Actor with ActorLogging {

  implicit val executor = context.system.dispatcher

  var populationActors: List[ActorRef] = List()

  self ! Start(0)

  def receive = {
    case msg: InEvent =>
      log.info("Starting generation")
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
      if (Random.nextInt(1000) < 1) {
        val r = Random.nextInt(populationActors.size)
        val destination =
          if (r == msg.index)
            (r + 1) % populationActors.size
          else
            r
        populationActors(msg.index) ! PopulationActor.Migrate(msg.index, Population.Size / 20, populationActors(destination))
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
