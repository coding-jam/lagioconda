package it.codingjam.lagioconda.actors

import akka.actor._
import akka.cluster.Cluster
import akka.routing.FromConfig
import it.codingjam.ga.protocol.Messages.CalculateFitness

/**
  * The connection point between frontend and  cluster
  */
class FitnessService extends Actor with ActorLogging {

  /** creating the cluster router  */
  val backend = context.actorOf(FromConfig.props(), name = "fitnessRouter")

  def receive = {
    case msg @ CalculateFitness(_, _) => backend forward msg
  }
}

object FitnessService {

  /**
    * Startup the service only if the cluster has the specified size
    */
  def startOn(system: ActorSystem) {
    Cluster(system) registerOnMemberUp {
      val service = system.actorOf(Props[FitnessService], name = "fitnessService")
      system.log info s"Fitness Service started at ${service.path}"
    }
  }
}
