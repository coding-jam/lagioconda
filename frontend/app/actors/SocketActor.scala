package it.codingjam.lagioconda.actors

import java.time.{Instant, Duration => JavaDuration}

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSelection, Props}
import it.codingjam.lagioconda.actors.PopulationActor.SetupPopulation
import it.codingjam.lagioconda.actors.SocketActor._
import it.codingjam.lagioconda.config.{Config, PopulationConfig}
import protocol.InEvent
import protocol.Messages.{Start, Statistics}

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

  FitnessService.startOn(context.system)

  val populationConfig = PopulationConfig.Default
  val config = Config.Default

  val service: ActorSelection = {
    val path = context.system / "fitnessService"
    context actorSelection path
  }

  context.system.scheduler.schedule(1.seconds, statisticsRate.milliseconds, self, PrintStatistics)

  override def receive = {
    case msg: InEvent =>
      Thread.sleep(1000)
      if (populationActors.length == 0) {
        log.info(s"Starting generation of $MaxPopulation populations")
        if (populationActors.isEmpty) {
          Range(0, MaxPopulation).foreach { i =>
            val a =
              context.actorOf(PopulationActor.props(service, out).withDispatcher("population-actor-dispatcher"), name = "pop" + i)
            populationActors = populationActors :+ a
            a ! SetupPopulation(i, config)
          }
        }
      }

    case msg: GenerationRan =>
      generationCounter += 1
      populationActors(msg.index) ! PopulationActor.RunAGeneration(msg.index)

    case msg: PopulationGenerated =>
      generationCounter += 1
      populationActors(msg.index) ! PopulationActor.RunAGeneration(msg.index)

    case msg @ SocketActor.PrintStatistics =>
      val d = JavaDuration.between(startedAt, Instant.now)
      val u = List(d.getSeconds)

      val rate = generationCounter.toDouble / MaxPopulation.toDouble / d.getSeconds
      oldGenerationCounter = generationCounter
      val output = List(
        s"Current generation: ${generationCounter / MaxPopulation}",
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
