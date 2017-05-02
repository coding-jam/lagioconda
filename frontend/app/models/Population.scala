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
                      hillClimbedGene: Int,
                      lastIncrement: Double,
                      trend: String = "") {

  implicit val to2 = akka.util.Timeout(20.seconds)

  def nextGeneration(a: ActorSelection, ec: ExecutionContext)(implicit fitnessFunction: FitnessFunction,
                                                              selection: SelectionFunction,
                                                              mutation: MutationPointLike,
                                                              dimension: ImageDimensions,
                                                              crossover: CrossoverPointLike,
                                                              temperature: Temperature): Population = {
    var temp = this.crossOver(a, ec)
    val oldBest = this.individuals.head
    var newBest = temp.individuals.head
    val bitsToMutate = 5

    if (generation > 10 && this.lastIncrement < 0.001 && (generation - newBestAtGeneration > 5)) {
      temp = temp.hillClimb(a, ec, temperature, bitsToMutate, temp.hillClimbedGene)
      newBest = temp.individuals.head
    }

    if (oldBest.fitness < newBest.fitness)
      Population(generation + 1,
                 temp.individuals,
                 temp.totalFitness,
                 generation + 1,
                 newBest.generatedBy,
                 temp.hillClimbedGene,
                 newBest.fitness - oldBest.fitness)
    else
      Population(generation + 1,
                 temp.individuals,
                 temp.totalFitness,
                 this.newBestAtGeneration,
                 oldBest.generatedBy,
                 temp.hillClimbedGene,
                 temp.lastIncrement)

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

    val chanceOfMutation = 5 * (1 - temperature.degrees)

    val mutationSize = Random.nextInt(6) + 1
    val times = (10 * (1 - temperature.degrees)).toInt // Math.min(1, (Chromosome.numberOfGenes * 0.2 * temperature.degrees)).toInt

    if (generation % 100 == 0)
      println("Temperature " + temperature.degrees)

    val mutationList: List[(Chromosome, String)] = offsprings.map { individual =>
      val r = Random.nextInt(100)

      if (r < chanceOfMutation)
        (individual.mutate(times)(mutation, mutationSize), "mutation")
      else
        (individual, "crossover")
    }

    val futures: immutable.Seq[Future[IndividualState]] = mutationList.map { chromosome =>
      (a ? CalculateFitness(chromosome._1, generation, chromosome._2)).mapTo[CalculatedFitness].map { cf =>
        IndividualState(cf.chromosome, cf.fitness, cf.reason)
      }
    }

    val future: Future[immutable.Seq[IndividualState]] = Future.sequence(futures)

    val l: List[IndividualState] = Await.result(future, 20.seconds).toList

    newIndividuals = newIndividuals ++ l

    val totalFitness: Double = newIndividuals.map(_.fitness).sum

    Population(generation, sort(newIndividuals), totalFitness, newBestAtGeneration, bestReason, hillClimbedGene, lastIncrement)
  }

  private def hillClimb(a: ActorSelection, ec: ExecutionContext, temperature: Temperature, bitsToMutate: Int, gene: Int)(
      implicit mutation: MutationPointLike): Population = {

    implicit val e = ec
    println("Hill climb with gene " + gene)

    var hillClimber = randomElite
    //individuals(Random.nextInt((individuals.size * temperature.degrees).toInt))

    val list = List(0, 8, 16, 22, 30, 38)
    val l = {
      val r = Random.nextInt(6)
      val g = list(r)
      val y = if (r == 2) 6 else 8
      val x = if (r == 2) 64 else 256

      val format = s"%0${y}d"

      Range(0, x)
        .map { number =>
          format.format(number.toBinaryString.toInt)
        }
        .map { chunk =>
          hillClimber.chromosome.neighbour(gene, g, chunk)
        }
        .toList
    }

    val f = fitness(a, l, generation, "hillClimb")
    val bestNeighbour = sort(f).head

    if (bestIndividual.fitness < bestNeighbour.fitness) {
      val selected = (List(bestNeighbour) ++ this.individuals).dropRight(1)
      val total = selected.map(_.fitness).sum
      println("Successfull Hill climb with gene " + gene)

      Population(generation,
                 selected,
                 total,
                 newBestAtGeneration,
                 bestReason = "hillClimb",
                 gene,
                 bestNeighbour.fitness - bestIndividual.fitness)
    } else {
      this.copy(hillClimbedGene = ((hillClimbedGene + 1) % Chromosome.numberOfGenes))
    }

  }

  private def fitness(a: ActorSelection, list: List[Chromosome], generation: Int, reason: String)(implicit ec: ExecutionContext) = {
    val futures: immutable.Seq[Future[IndividualState]] = list.map { chromosome =>
      (a ? CalculateFitness(chromosome, generation, reason)).mapTo[CalculatedFitness].map { cf =>
        IndividualState(cf.chromosome, cf.fitness, cf.reason)
      }
    }
    val future: Future[immutable.Seq[IndividualState]] = Future.sequence(futures)
    Await.result(future, 20.seconds).toList
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

  def randomNotElite: IndividualState = individuals(Population.EliteCount + Random.nextInt(Population.Size - Population.EliteCount))

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
    val l = list.map(i => IndividualState(i.chromosome, i.fitness, "migration"))

    val individuals = (this.individuals ++ l).sorted(Ordering[IndividualState]).reverse.take(Population.Size)
    val total = individuals.map(_.fitness).sum
    Population(generation, individuals, total, newBestAtGeneration, bestReason, hillClimbedGene, lastIncrement)
  }

}

object Population {

  val Size = 40
  val EliteCount = 3
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

    Population(0, list.sorted(Ordering[IndividualState].reverse), list.map(_.fitness).sum, 0, "random", 0, 0.0)
  }

}
