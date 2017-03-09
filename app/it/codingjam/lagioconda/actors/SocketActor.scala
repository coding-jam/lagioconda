package it.codingjam.lagioconda.actors

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import it.codingjam.lagioconda.actors.PopulationActor.{MigrationDone, SetupPopulation}
import it.codingjam.lagioconda.actors.SocketActor._
import it.codingjam.lagioconda.protocol.InEvent
import it.codingjam.lagioconda.protocol.Messages.Start
import it.codingjam.lagioconda.services.ImageGenerator

import scala.util.Random
import scala.concurrent.duration._

class SocketActor(out: ActorRef, imageGenerator: ImageGenerator) extends Actor with ActorLogging {

  implicit val executor = context.system.dispatcher

  val MaxPopulation = 10

  var populationActors: List[ActorRef] = List()
  var generationCounter = 0
  var oldGenerationCounter = 0
  val statisticsRate = 10

  self ! Start(0)

  context.system.scheduler.schedule(5.seconds, statisticsRate.seconds, self, PrintStatistics)

  override def receive = {
    case msg: InEvent =>
      if (populationActors.length == 0) {
        log.info(s"Starting generation of $MaxPopulation populations")
        if (populationActors.isEmpty) {
          Range(0, MaxPopulation).foreach { i =>
            val a =
              context.actorOf(PopulationActor
                                .props(out)
                                .withDispatcher("population-actor-dispatcher"),
                              name = "pop" + i)
            populationActors = populationActors :+ a
            a ! SetupPopulation(i)
          }
        }
      }

    case msg: GenerationRan =>
      generationCounter += 1
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
      generationCounter += 1
      populationActors(msg.index) ! PopulationActor.RunAGeneration(msg.index)

    case msg: MigrationDone =>
      populationActors(msg.index) ! PopulationActor.RunAGeneration(msg.index)

    case msg @ PrintStatistics =>
      val rate = (generationCounter - oldGenerationCounter) / (statisticsRate.toDouble)
      oldGenerationCounter = generationCounter
      log.info(s"Rate: Generation/s = ${rate}")

    case other =>
      log.error(s"This must not happen $other")
  }
}

object SocketActor {
  def props(out: ActorRef, ig: ImageGenerator) =
    Props(new SocketActor(out, ig))

  case object GenerateAction

  case object PrintStatistics

  case class PopulationGenerated(index: Int, generation: Int)

  case class GenerationRan(index: Int, generation: Int)

}
