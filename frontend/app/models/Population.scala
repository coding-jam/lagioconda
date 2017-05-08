package it.codingjam.ga

import akka.actor.ActorSelection
import akka.pattern.ask
import com.typesafe.scalalogging.LazyLogging
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
                      lastResults: List[Double],
                      lastMigrationFrom: Int,
                      trend: String = "")
    extends LazyLogging {

  implicit val to2 = akka.util.Timeout(20.seconds)

  def rotate(list: List[Double], double: Double) = (list :+ double).takeRight(5)

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

    if (oldBest.fitness < newBest.fitness && this.generation % 5000000L == 0) {
      temp = temp.hillClimb(a, ec, temperature, bitsToMutate, temp.hillClimbedGene)
      temp = temp.copy(lastResults = List())
      newBest = temp.individuals.head
    }

    if (oldBest.fitness < newBest.fitness) {
      val lastI = newBest.fitness - oldBest.fitness
      Population(
        generation + 1,
        temp.individuals,
        temp.totalFitness,
        generation + 1,
        newBest.generatedBy,
        temp.hillClimbedGene,
        lastI,
        rotate(temp.lastResults, lastI),
        this.lastMigrationFrom
      )
    } else
      Population(
        generation + 1,
        temp.individuals,
        temp.totalFitness,
        this.newBestAtGeneration,
        oldBest.generatedBy,
        temp.hillClimbedGene,
        temp.lastIncrement,
        rotate(temp.lastResults, 0.0),
        lastMigrationFrom
      )

  }

  def doMigration(migration: List[IndividualState], a: ActorSelection, ec: ExecutionContext, otherPopulationIndex: Int)(
      implicit fitnessFunction: FitnessFunction,
      selection: SelectionFunction,
      mutation: MutationPointLike,
      dimension: ImageDimensions,
      crossover: CrossoverPointLike,
      temperature: Temperature): Population = {
    logger.debug("Migration from population " + otherPopulationIndex)

    var temp = this.migration(migration, a, ec)
    val oldBest = this.individuals.head
    var newBest = temp.individuals.head

    if (oldBest.fitness < newBest.fitness) {
      val lastI = newBest.fitness - oldBest.fitness
      Population(
        generation + 1,
        temp.individuals,
        temp.totalFitness,
        generation + 1,
        newBest.generatedBy,
        temp.hillClimbedGene,
        newBest.fitness - oldBest.fitness,
        rotate(lastResults, lastI),
        otherPopulationIndex
      )
    } else
      Population(
        generation + 1,
        temp.individuals,
        temp.totalFitness,
        this.newBestAtGeneration,
        oldBest.generatedBy,
        temp.hillClimbedGene,
        temp.lastIncrement,
        rotate(lastResults, 0.0),
        otherPopulationIndex
      )

  }

  def migration(list: List[IndividualState], a: ActorSelection, ec: ExecutionContext)(implicit fitnessFunction: FitnessFunction,
                                                                                      selection: SelectionFunction,
                                                                                      dimension: ImageDimensions,
                                                                                      mutation: MutationPointLike,
                                                                                      crossover: CrossoverPointLike,
                                                                                      temperature: Temperature): Population = {

    implicit val e = ec

    val splitted = individuals.splitAt(Population.EliteCount)
    var newIndividuals = splitted._1 // start with elite
    val choice = Random.nextInt(list.size)

    val offsprings = Range(0, Population.Size * 2).map { i =>
      val selected1 = list(choice)
      val selected2 = selection.select(this)
      selected1.chromosome.uniformCrossover(selected2.chromosome)
    }.toList

    val migrationList: List[(Chromosome, String)] = offsprings.map { individual =>
      (individual, "migration")
    }

    val futures: immutable.Seq[Future[IndividualState]] = migrationList.map { chromosome =>
      (a ? CalculateFitness(chromosome._1, generation, chromosome._2)).mapTo[CalculatedFitness].map { cf =>
        IndividualState(cf.chromosome, cf.fitness, cf.reason)
      }
    }

    val future: Future[immutable.Seq[IndividualState]] = Future.sequence(futures)

    val l: List[IndividualState] = Await.result(future, 20.seconds).toList

    newIndividuals = newIndividuals ++ l

    val totalFitness: Double = newIndividuals.map(_.fitness).sum

    Population(generation,
               sort(newIndividuals),
               totalFitness,
               newBestAtGeneration,
               bestReason,
               hillClimbedGene,
               lastIncrement,
               lastResults,
               lastMigrationFrom)
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
    val offsprings = Range(0, Population.Size).map { i =>
      val selected1: IndividualState = selection.select(this)
      var selected2 = selection.select(this)
      while (selected1 == selected2) selected2 = selection.select(this)
      selected1.chromosome.uniformCrossover(selected2.chromosome)
    }.toList

    val chanceOfMutation = 5
    val mutationSize = 1
    val times = Random.nextInt(3) + 1

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

    Population(generation,
               sort(newIndividuals),
               totalFitness,
               newBestAtGeneration,
               bestReason,
               hillClimbedGene,
               lastIncrement,
               lastResults,
               lastMigrationFrom)
  }

  def neighbour(chromosome: Chromosome, gene: Int): List[Chromosome] = {
    it.codingjam.lagioconda.conversions.neigh(chromosome.genes(gene)).map { g =>
      Chromosome(chromosome.genes.slice(0, gene) ++ List(g) ++ chromosome.genes.slice(gene + 1, chromosome.genes.length))
    }
  }

  private def hillClimb(a: ActorSelection, ec: ExecutionContext, temperature: Temperature, bitsToMutate: Int, gene: Int)(
      implicit mutation: MutationPointLike): Population = {

    implicit val e = ec
    println("Hill climb with gene " + gene)

    var hillClimber = bestIndividual
    //individuals(Random.nextInt((individuals.size * temperature.degrees).toInt))

    val l: List[Chromosome] = neighbour(hillClimber.chromosome, gene)

    val f = fitness(a, l, generation, "hillClimb")
    val bestNeighbour = sort(f).head

    if (bestIndividual.fitness < bestNeighbour.fitness) {
      val selected = (List(bestNeighbour) ++ this.individuals).dropRight(1)
      val total = selected.map(_.fitness).sum
      println("Successfull Hill climb with gene " + gene)
      val lastI = bestNeighbour.fitness - bestIndividual.fitness

      Population(generation,
                 selected,
                 total,
                 newBestAtGeneration,
                 bestReason = "hillClimb",
                 gene,
                 lastI,
                 rotate(lastResults, lastI),
                 lastMigrationFrom)
    } else {
      this.copy(hillClimbedGene = ((hillClimbedGene + 1) % Chromosome.numberOfGenes), lastResults = rotate(lastResults, 0.0))
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

}

object Population {

  val Size = 20
  val EliteCount = 4
  val IncrementBeforeCut = (Size * 10.0 / 100.0).toInt
  //val NumberOfMutatingGenes: Int = (Size * 50.0 / 100.0).toInt

  def randomGeneration()(implicit fitnessFunction: FitnessFunction, dimension: ImageDimensions, configuration: Configuration): Population = {

    var list: List[IndividualState] = List()

    Range(0, Size).foreach { i =>
      val c: Chromosome = RandomChromosome.generate(Gene.Size)
      val fitness = fitnessFunction.fitness(c)
      val individual = IndividualState(c, fitness, "random")
      list = list :+ individual
    }

    Population(0, list.sorted(Ordering[IndividualState].reverse), list.map(_.fitness).sum, 0, "random", 0, 0.0, List(), 0)
  }

}
