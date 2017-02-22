package it.codingjam.lagioconda.actors

import akka.actor.ActorRef

import scala.util.Random

case class MasterState(population: List[IndividualState]) {

  def addIndividual(individualState: IndividualState): MasterState =
    copy(population = population :+ individualState)

  def removeIndividual(actorRef: ActorRef): MasterState =
    copy(population = population.filterNot(x => x.actorRef == actorRef))

  def sort: MasterState = copy(population = population.sorted)

  def reverseSort: MasterState =
    copy(population = population.sorted(Ordering[IndividualState].reverse))

  def randomIndividual: IndividualState =
    population(Random.nextInt(population.size / 10))

  def removeLast(i: Int): MasterState =
    copy(population = population.dropRight(i))

  def meanFitness: Double = population.map(_.fitness).sum / population.size

}
