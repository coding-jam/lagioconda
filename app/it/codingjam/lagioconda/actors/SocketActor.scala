package it.codingjam.lagioconda.actors

import akka.actor.{Props, Actor, ActorRef}
import it.codingjam.lagioconda.actors.MasterActor.{
  Mutate,
  RemoveWeaker,
  SetupPopulation,
  Crossover
}
import it.codingjam.lagioconda.actors.SocketActor.{
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
  println("go go ")

  self ! Start(0)

  def receive = {
    case msg: InEvent =>
      println("CMD" + msg)
      if (masterActor.isEmpty) {
        masterActor = Some(
          context.actorOf(MasterActor.props(out), name = "master"))
        masterActor.foreach(a => a ! SetupPopulation)

      }
    case msg @ GenerateAction =>
      Range(1, 100).foreach { i =>
        val l = Random.nextInt(2000)
        if (l < 100) {
          masterActor.foreach(a => a ! RemoveWeaker)
        } else if (l < 1500) {
          masterActor.foreach(a => a ! Mutate)
        } else {
          masterActor.foreach(a => a ! Crossover)
        }
      }

    case PopulationGenerated =>
      this.context.system.scheduler
        .schedule(5000.milliseconds, 1.millisecond, self, GenerateAction)

    case x =>
      println("CMD" + x)
      if (masterActor.isEmpty) {
        masterActor = Some(
          context.actorOf(MasterActor.props(out), name = "master"))
        masterActor.foreach(a => a ! Start)

        this.context.system.scheduler
          .schedule(5000.milliseconds, 1500.millisecond, self, GenerateAction)

      }
  }
}

object SocketActor {
  def props(out: ActorRef, ig: ImageGenerator) =
    Props(new SocketActor(out, ig))

  case object GenerateAction

  case object PopulationGenerated
}
