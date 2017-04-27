package it.codingjam.ga

import akka.actor.ActorSelection
import akka.pattern.ask
import it.codingjam.ga.protocol.Messages.{CalculateFitness, CalculatedFitness}
import it.codingjam.lagioconda.domain.{Configuration, ImageDimensions}
import it.codingjam.lagioconda.fitness.FitnessFunction
import it.codingjam.lagioconda.ga._
import it.codingjam.lagioconda.models.IndividualState

import scala.collection.immutable
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Random

case class Population(generation: Int,
                      individuals: List[IndividualState],
                      totalFitness: Double,
                      newBestAtGeneration: Int,
                      bestReason: String,
                      trend: String = "") {

  implicit val to2 = akka.util.Timeout(3.seconds)

  def nextGeneration(a: ActorSelection, ec: ExecutionContext)(implicit fitnessFunction: FitnessFunction,
                                                              selection: SelectionFunction,
                                                              mutation: MutationPointLike,
                                                              dimension: ImageDimensions,
                                                              crossover: CrossoverPointLike,
                                                              temperature: Temperature): Population = {
    var temp = this.crossOver(a, ec)
    val oldBest = this.individuals.head
    var newBest = temp.individuals.head

    if (generation - newBestAtGeneration > 10) {
      temp = temp.hillclimb(a, ec, temperature, 5, generation % Chromosome.numberOfGenes)
      newBest = temp.individuals.head
    }

    if (oldBest.fitness < newBest.fitness)
      Population(generation + 1, temp.individuals, temp.totalFitness, generation + 1, newBest.generatedBy)
    else
      Population(generation + 1, temp.individuals, temp.totalFitness, this.newBestAtGeneration, oldBest.generatedBy)

  }

  def crossOver(a: ActorSelection, ec: ExecutionContext)(implicit fitnessFunction: FitnessFunction,
                                                         selection: SelectionFunction,
                                                         dimension: ImageDimensions,
                                                         mutation: MutationPointLike,
                                                         crossover: CrossoverPointLike,
                                                         temperature: Temperature): Population = {

    implicit val e = ec

    val splitted = individuals.splitAt(Population.EliteCount)
    var newIndividuals = splitted._1 // start with elite
    val offsprings = Range(Population.EliteCount, Population.Size).map { i =>
      val selected = selection.select(this)
      individuals(i).chromosome.uniformCrossover(selected.chromosome)
    }.toList

    val chanceOfMutation = 100 * temperature.degrees

    val geneMutation = (120 * temperature.degrees).toInt
    val numberOfGenes = (Chromosome.numberOfGenes * 0.4 + temperature.degrees).toInt

    val mutationList: List[Chromosome] = offsprings.map { individual =>
      val r = Random.nextInt(100)

      if (r < chanceOfMutation)
        individual.mutate(numberOfGenes)(mutation, geneMutation)
      else
        individual
    }

    val futures: immutable.Seq[Future[IndividualState]] = mutationList.map { chromosome =>
      (a ? CalculateFitness(chromosome, generation)).mapTo[CalculatedFitness].map { cf =>
        IndividualState(cf.chromosome, cf.fitness, "m/c")
      }
    }

    val future: Future[immutable.Seq[IndividualState]] = Future.sequence(futures)

    val l: List[IndividualState] = Await.result(future, 5.seconds).toList

    newIndividuals = newIndividuals ++ l

    val totalFitness: Double = newIndividuals.map(_.fitness).sum

    Population(generation, sort(newIndividuals), totalFitness, newBestAtGeneration, bestReason)
  }

  def mutation(a: ActorSelection, ec: ExecutionContext)(implicit fitnessFunction: FitnessFunction,
                                                        mutation: MutationPointLike,
                                                        temperature: Temperature): Population = {

    implicit val e = ec

    val splitted = individuals.splitAt(Population.EliteCount)
    val elite = splitted._1 // start with elite

    val chanceOfMutation = 100 * temperature.degrees

    val geneMutation = (120 * temperature.degrees).toInt
    val numberOfGenes = (Chromosome.numberOfGenes * 0.4 + temperature.degrees).toInt

    val list: List[Either[Chromosome, IndividualState]] = Range(Population.EliteCount, Population.Size).map { i =>
      val individual = individuals(i)
      val r = Random.nextInt(100)

      if (r < chanceOfMutation)
        Left(individual.chromosome.mutate(numberOfGenes)(mutation, geneMutation))
      else
        Right(individual)
    }.toList

    val l: (List[Chromosome], List[IndividualState]) = splitEitherList(list)

    val f = fitness(a, l._1, generation, "Mutation")

    val newIndividuals = elite ++ f ++ l._2

    val totalFitness: Double = newIndividuals.map(_.fitness).sum

    Population(generation, sort(newIndividuals), totalFitness, newBestAtGeneration, bestReason)

  }

  def hillclimb(a: ActorSelection, ec: ExecutionContext, temperature: Temperature, length: Int, gene: Int)(
      implicit mutation: MutationPointLike): Population = {

    implicit val e = ec

    var hillClimber = individuals(Random.nextInt((individuals.size * temperature.degrees).toInt))

    val l = Range(0, length).map { i =>
      hillClimber.chromosome.neighbour(gene, (5 * temperature.degrees).toInt)
    }.toList

    val f = fitness(a, l, generation, "hillclimb")
    val bestNeightbour = sort(f).head

    if (bestIndividual.fitness < bestNeightbour.fitness) {
      val selected = (List(bestNeightbour) ++ this.individuals).dropRight(1)
      val total = selected.map(_.fitness).sum

      // println("Hill clim new best at " + (pop.generation))
      Population(generation, selected, total, newBestAtGeneration, bestReason = "hillclimb")
    } else {
      this
    }

  }

  private def fitness(a: ActorSelection, list: List[Chromosome], generation: Int, reason: String)(implicit ec: ExecutionContext) = {
    val futures: immutable.Seq[Future[IndividualState]] = list.map { chromosome =>
      (a ? CalculateFitness(chromosome, generation)).mapTo[CalculatedFitness].map { cf =>
        IndividualState(cf.chromosome, cf.fitness, reason)
      }
    }
    val future: Future[immutable.Seq[IndividualState]] = Future.sequence(futures)
    Await.result(future, 5.seconds).toList
  }

  private def splitEitherList[A, B](el: List[Either[A, B]]) = {
    val (lefts, rights) = el.partition(_.isLeft)
    (lefts.map(_.left.get), rights.map(_.right.get))
  }

  private def sort(list: List[IndividualState]) = list.sorted(Ordering[IndividualState]).reverse

  def addIfNotClone(list: List[IndividualState], newIndividual: IndividualState) = {
    val l: Seq[IndividualState] = list.filter(i => i.fitness == newIndividual.fitness)
    val m: Seq[Set[Gene]] = l.map(i => i.chromosome.genes.toSet)
    val n = m.find(set => set.equals(newIndividual.chromosome.genes.toSet))
    if (n.isDefined)
      list
    else list :+ newIndividual
  }

  def bestIndividual = individuals.head

  def randomIndividual: IndividualState =
    individuals(Random.nextInt(individuals.size))

  def randomElite: IndividualState = individuals(Random.nextInt(Population.EliteCount))

  def randomPositionAndIndividual: (Int, IndividualState) = {
    val pos = Random.nextInt(individuals.size)
    (pos, individuals(pos))
  }

  def randomIndividualInRange(position: Int) = {

    def normalizedPosition(i: Int) = {
      if (i < 0) 0
      else if (i > individuals.size - 1) individuals.size - 1
      else i
    }

    val range = 12
    val pos = position - (range / 2) + Random.nextInt(range)
    individuals(normalizedPosition(pos))
  }

  def randomIndividualByWeight: IndividualState = {
    val total = (individuals.size * (individuals.size + 1)) / 2
    var r = Random.nextInt(total)
    var x = individuals.size
    while (r > 0) {
      r = r - x
      x = x - 1
    }
    val position = individuals.size - x - 1
    individuals(if (position >= 0) position else 0)
  }

  def meanFitness: Double = totalFitness / individuals.size

  def addIndividuals(list: List[IndividualState]) = {
    val individuals = (this.individuals ++ list).sorted(Ordering[IndividualState]).reverse
    Population(generation, individuals.take(Population.Size), 0.0, newBestAtGeneration, bestReason)
  }

}

object Population {

  val Size = 20
  val EliteCount = 4
  val IncrementBeforeCut = (Size * 10.0 / 100.0).toInt
  //val NumberOfMutatingGenes: Int = (Size * 50.0 / 100.0).toInt

  def randomGeneration()(implicit fitnessFunction: FitnessFunction, dimension: ImageDimensions, configuration: Configuration): Population = {

    var list: List[IndividualState] = List()

    Range(0, Size).foreach { i =>
      val c: Chromosome = RandomChromosome.generate(47)
      val fitness = fitnessFunction.fitness(c)
      val individual = IndividualState(c, fitness, "random")
      list = list :+ individual
    }

    Population(0, list.sorted(Ordering[IndividualState].reverse), list.map(_.fitness).sum, 0, "random")
  }

}
