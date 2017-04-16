package it.codingjam.lagioconda.actors

import java.time.{Duration => JavaDuration, Instant}

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import it.codingjam.lagioconda.actors.PopulationActor.{MigrationDone, SetupPopulation}
import it.codingjam.lagioconda.actors.SocketActor._
import it.codingjam.lagioconda.protocol.InEvent
import it.codingjam.lagioconda.protocol.Messages.{Start, Statistics}

import scala.concurrent.duration._

class SocketActor(out: ActorRef) extends Actor with ActorLogging {

  implicit val executor = context.system.dispatcher

  val MaxPopulation = 1

  var populationActors: List[ActorRef] = List()
  var generationCounter = 0
  var oldGenerationCounter = 0
  val statisticsRate = 500

  val startedAt: Instant = Instant.now()
  self ! Start(0)

  context.system.scheduler.schedule(1.seconds, statisticsRate.milliseconds, self, PrintStatistics)

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
      /*
      if (Random.nextInt(10000000000000000) < 1) {
        val r = Random.nextInt(populationActors.size)
        val destination =
          if (r == msg.index)
            (r + 1) % populationActors.size
          else
            r
        populationActors(msg.index) ! PopulationActor.Migrate(msg.index, Population.Size / 20, populationActors(destination))

      } else
       */
      populationActors(msg.index) ! PopulationActor.RunAGeneration(msg.index)

    case msg: PopulationGenerated =>
      generationCounter += 1
      populationActors(msg.index) ! PopulationActor.RunAGeneration(msg.index)

    case msg: MigrationDone =>
      populationActors(msg.index) ! PopulationActor.RunAGeneration(msg.index)

    case msg @ PrintStatistics =>
      val rate = (generationCounter - oldGenerationCounter) * (1000.0 / statisticsRate.toDouble)
      oldGenerationCounter = generationCounter
      val output = List(
        s"Current generation: ${generationCounter}",
        s"Rate: Generation/s = ${rate}",
        timeDifference()
      ).mkString("<br/>")
      out ! Statistics(output)

    case other =>
      log.error(s"This must not happen $other")
  }

  def timeDifference(): String = {
    val d = JavaDuration.between(startedAt, Instant.now)
    val u = List(d.getSeconds)

    timeConversion(u.head)
  }

  private def timeConversion(totalSeconds: Long) = {
    val MinutesInHour = 60
    val SecondsInMinutes = 60
    val seconds = totalSeconds % SecondsInMinutes
    val totalMinutes = totalSeconds / SecondsInMinutes
    val minutes = totalMinutes % MinutesInHour
    val hours = totalMinutes / MinutesInHour
    hours + " hours " + minutes + " minutes " + seconds + " seconds"
  }

}

object SocketActor {
  def props(out: ActorRef) =
    Props(new SocketActor(out))

  case object GenerateAction

  case object PrintStatistics

  case class PopulationGenerated(index: Int, generation: Int)

  case class GenerationRan(index: Int, generation: Int)

}
