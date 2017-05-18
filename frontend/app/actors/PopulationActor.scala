package it.codingjam.lagioconda.actors

import java.awt.image.BufferedImage
import java.io.{ByteArrayOutputStream, File}
import javax.imageio.ImageIO

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSelection, Props}
import it.codingjam.lagioconda.actors.PopulationActor.{RunHillClimb, SetupPopulation}
import it.codingjam.lagioconda.actors.SocketActor.{GenerationRan, PopulationGenerated}
import it.codingjam.lagioconda.config.Config
import it.codingjam.lagioconda.conversions.ChromosomeToBufferedImage
import it.codingjam.lagioconda.fitness.ByteComparisonFitness
import it.codingjam.lagioconda.ga.{MutationPointLike, _}
import it.codingjam.lagioconda.population.{Individual, Population}
import it.codingjam.lagioconda.protocol.Messages.IndividualInfo
import it.codingjam.lagioconda.selection.WheelSelection
import it.codingjam.lagioconda.{FitnessCalculator, ImageDimensions, PopulationOps}
import org.apache.commons.codec.binary.Base64OutputStream

import scala.util.Random

class PopulationActor(service: ActorSelection, out: ActorRef) extends Actor with ActorLogging {

  private var state = Population(0, List[Individual](), 0, List())
  private var index = -1
  private implicit val scheduler = context.system.scheduler
  private implicit var config: Config = Config.Default
  private implicit val ec = this.context.dispatcher

  private val file = new File("frontend/public/images/monalisasmall2.png")

  private val reference = ImageIO.read(file)

  private val convertedImg: BufferedImage = new BufferedImage(reference.getWidth(), reference.getHeight(), BufferedImage.TYPE_3BYTE_BGR);
  convertedImg.getGraphics().drawImage(reference, 0, 0, null)

  private implicit val dimension = ImageDimensions(reference.getWidth, reference.getHeight)
  private implicit val crossover = new RandomCrossoverPoint
  private implicit var mutation: MutationPointLike = new RandomMutationPoint
  private implicit val selection = new WheelSelection
  private implicit val fitness = new ByteComparisonFitness(convertedImg, dimension)

  private implicit val fitnessCalculator = FitnessCalculator(service, fitness, dimension)

  private var best: Option[Individual] = None
  private var initialBest = 0.0
  private implicit var temperature = Temperature(1.0)

  private def formatPercent(d: Double) = f"$d%1.5f"

  private def formatFitness(d: Double) = f"$d%1.3f"

  override def receive: Receive = {
    case cmd: PopulationActor.SetupPopulation =>
      state = PopulationOps.randomGeneration()
      config = cmd.config
      index = cmd.index
      best = state.individuals.headOption
      best.foreach(updateUI(cmd.index, _, 0.0, 0.0, 0))

      initialBest = state.individuals.head.fitness
      log.debug("Initial Best {}", initialBest)
      sender() ! PopulationGenerated(cmd.index, state.generation)

    case cmd: PopulationActor.RunAGeneration =>
      val oldBest: Option[Individual] = best
      implicit val ec = context.dispatcher

      state = PopulationOps.crossover(state)

      temperature = Temperature(((1.0 - state.individuals.head.fitness) / (1.0 - initialBest)))

      best = state.individuals.headOption
      best.foreach { b =>
        oldBest.foreach { old =>
          if (b.fitness > old.fitness) {
            updateUI(cmd.index, b, b.fitness - old.fitness, old.fitness, 0)
          }
        }
      }

      println("state.lastResults.sum " + state.lastResults.sum)
      println("state.lastResults.length " + state.lastResults.length)
      if (state.lastResults.sum < 0.0001 && state.lastResults.length >= Population.MaxRotate) {
        log.debug("Starting hill climb")
        self ! RunHillClimb
      } else {
        sender() ! GenerationRan(index, state.generation)
      }

    case PopulationActor.RunHillClimb =>
      log.debug("Hill climb")
      var useHc = true
      val size = best.get.chromosome.genes.size
      var temp = state
      var newBest = temp.individuals.head
      while (useHc) {
        val start = if (Random.nextInt(20) < 1) 0 else Math.max(0, size - 1)

        Range(start, size).map { gene =>
          var oldFitness = temp.bestIndividual.fitness
          temp = PopulationOps.hillClimb(temp, gene)
          if (temp.bestIndividual.fitness > oldFitness) {
            val newFitness = temp.bestIndividual.fitness
            updateUI(index, temp.bestIndividual, newFitness - oldFitness, oldFitness, 0)
            oldFitness = temp.bestIndividual.fitness
            log.debug("Hill climb successful " + newFitness)
            useHc = true
          } else {
            useHc = false
          }
        }
        newBest = temp.individuals.head
      }

      temp = temp.copy(lastResults = List())
      val fBeforeAdd = temp.bestIndividual.fitness
      temp = PopulationOps.addGene(temp)
      updateUI(index, temp.bestIndividual, 0, fBeforeAdd, 0)
      state = temp
      best = state.individuals.headOption
      sender() ! GenerationRan(index, state.generation)
  }

  def updateUI(populationIndex: Int, best: Individual, increment: Double, oldFitness: Double, otherPopulationIndex: Int): Unit = {
    val bi = best.chromosome.toBufferedImage()

    val os = new ByteArrayOutputStream()
    val b64 = new Base64OutputStream(os)

    ImageIO.write(bi, "png", b64)
    val image = os.toString("UTF-8")

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

  case object RunHillClimb

}
