package it.codingjam.lagioconda.actors

import java.awt.image.BufferedImage
import java.io.{ByteArrayOutputStream, File}
import javax.imageio.ImageIO

import actors.ComputationContext
import akka.actor.{Actor, ActorLogging, ActorRef, ActorSelection, PoisonPill, Props}
import it.codingjam.lagioconda.actors.PopulationActor.RunHillClimb
import it.codingjam.lagioconda.actors.SocketActor.{GenerationRan, PopulationGenerated}
import it.codingjam.lagioconda.config.Config
import it.codingjam.lagioconda.conversions.ChromosomeToBufferedImage
import it.codingjam.lagioconda.fitness.ByteComparisonFitness
import it.codingjam.lagioconda.ga._
import it.codingjam.lagioconda.population.{Individual, Population}
import it.codingjam.lagioconda.{FitnessCalculator, GeneMapping, ImageDimensions, PopulationOps}
import org.apache.commons.codec.binary.Base64OutputStream
import protocol.Messages.IndividualInfo

import scala.util.Random

class PopulationActor(service: ActorSelection, out: ActorRef) extends Actor with ActorLogging {

  private var state = Population(0, List[Individual](), 0, List())
  private var index = -1
  private implicit val scheduler = context.system.scheduler
  private implicit var config: Config = _
  private implicit val ec = this.context.dispatcher

  private val file = new File("frontend/public/images/monalisa.png")

  private val reference = ImageIO.read(file)

  private val convertedImg: BufferedImage = new BufferedImage(reference.getWidth(), reference.getHeight(), BufferedImage.TYPE_3BYTE_BGR);
  convertedImg.getGraphics().drawImage(reference, 0, 0, null)

  private implicit val dimension = ImageDimensions(reference.getWidth, reference.getHeight)
  private implicit val fitness = new ByteComparisonFitness(convertedImg, dimension)
  private var computation = new ComputationContext

  private implicit var fitnessCalculator: FitnessCalculator = _

  private var initialBest = 0.0

  private def formatPercent(d: Double) = f"$d%1.5f"
  private def formatFitness(d: Double) = f"$d%1.3f"
  private def currentTemperature = Temperature(((1.0 - state.individuals.head.fitness) / (1.0 - initialBest)))

  override def receive: Receive = {
    case cmd: PopulationActor.SetupPopulation =>
      config = cmd.config
      fitnessCalculator = FitnessCalculator(service, fitness, dimension, cmd.config.alpha)
      state = PopulationOps.randomGeneration()
      index = cmd.index
      computation = computation.copy(best = state.individuals.headOption)
      computation.best.foreach(updateUI(cmd.index, _, 0.0, 0.0, 0))
      initialBest = state.individuals.head.fitness
      log.debug(s"Initial Best $initialBest")
      sender() ! PopulationGenerated(cmd.index, state.generation)

    case cmd: PopulationActor.RunAGeneration =>
      val oldBest = state.bestIndividual
      state = PopulationOps.crossover(state)

      val result = if (state.bestIndividual > oldBest) {
        state.bestIndividual.fitness - oldBest.fitness
      } else {
        0.0d
      }

      computation = computation.copy(
        best = state.individuals.headOption,
        temperature = currentTemperature,
        lastResults = rotate(computation.lastResults, result)
      )

      eventuallyUpdateUi(oldBest)

      if (config.hillClimb.active && computation.lastResults.sum < config.hillClimb.slopeHeight && computation.lastResults.length >= config.hillClimb.slopeSize) {
        self ! RunHillClimb(sender())
      } else {
        sender() ! GenerationRan(index, state.generation)
      }

    case cmd: PopulationActor.RunHillClimb =>
      var continue = true
      val size = state.bestIndividual.chromosome.genes.size
      var temp = state
      var newBest = temp.individuals.head
      while (continue) {
        val start = if (Random.nextInt(100) < config.hillClimb.fullGeneHillClimbChange) 0 else Math.max(0, size - 1)

        Range(start, size).map { gene =>
          var oldFitness = temp.bestIndividual.fitness
          temp = PopulationOps.hillClimb(temp, gene)
          if (temp.bestIndividual.fitness > oldFitness) {
            val newFitness = temp.bestIndividual.fitness
            updateUI(index, temp.bestIndividual, newFitness - oldFitness, oldFitness, 0)
            oldFitness = temp.bestIndividual.fitness
            log.debug(s"Hill climb successful $newFitness")
            continue = true
          } else {
            continue = false
          }
        }
        newBest = temp.individuals.head
      }

      computation = computation.copy(lastResults = List())
      val fBeforeAdd = temp.bestIndividual.fitness
      if (config.hillClimb.addGene) {
        temp = PopulationOps.addGene(temp)
        updateUI(index, temp.bestIndividual, 0, fBeforeAdd, 0)
      }
      state = temp
      computation = computation.copy(best = state.individuals.headOption)

      cmd.socket ! GenerationRan(index, state.generation)

    case unknown =>
      log.error("Unknown message {}", unknown)
      self ! PoisonPill
  }

  private def eventuallyUpdateUi(oldBest: Individual): Unit = {
    computation.best.foreach { b =>
      if (b.fitness > oldBest.fitness) {
        updateUI(index, b, b.fitness - oldBest.fitness, oldBest.fitness, 0)
      }
    }
  }

  private def rotate(list: List[Double], double: Double) = (list :+ double).takeRight(Population.MaxRotate)

  private def updateUI(populationIndex: Int, best: Individual, increment: Double, oldFitness: Double, otherPopulationIndex: Int): Unit = {
    val bufferedImage: BufferedImage = best.chromosome.toBufferedImage(config.alpha)
    val outputStream = new ByteArrayOutputStream()
    val base64OutputStream: Base64OutputStream = new Base64OutputStream(outputStream)

    ImageIO.write(bufferedImage, "png", base64OutputStream)
    val image = outputStream.toString("UTF-8")

    val message =
      s"""Fit: ${formatFitness(best.fitness * 100)}%
         |@ g: ${best.age}, reason ${best.generatedBy}, </b>
         |genes: ${best.chromosome.genes.length}""".stripMargin
    log.debug(
      "Population {}, reason {}, old fitness {}, increment {}",
      populationIndex + "/" + best.age,
      best.generatedBy,
      formatPercent(oldFitness * 100),
      formatPercent(increment * 100)
    )

    out ! IndividualInfo(generation = best.age, image = image, population = index, info = message)
  }
}

object PopulationActor {

  def props(service: ActorSelection, out: ActorRef): Props = Props(new PopulationActor(service, out))

  case class SetupPopulation(index: Int, config: Config)

  case class RunAGeneration(index: Int)

  case class RunHillClimb(socket: ActorRef)

}
