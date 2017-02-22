package it.codingjam.lagioconda.actors

import akka.actor.ActorRef

case class IndividualState(actorRef: ActorRef, fitness: Double)
    extends Ordered[IndividualState] {

  def compare(that: IndividualState): Int = fitness.compareTo(that.fitness)

}
