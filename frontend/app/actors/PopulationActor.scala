package it.codingjam.lagioconda.actors

import java.awt.image.BufferedImage
import java.io.{ByteArrayOutputStream, File}
import javax.imageio.ImageIO

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSelection, Props}
import it.codingjam.lagioconda.actors.PopulationActor.{Migrate, Migration, MigrationDone, SetupPopulation}
import it.codingjam.lagioconda.actors.SocketActor.{GenerationRan, PopulationGenerated}
import it.codingjam.lagioconda.conversions.ChromosomeToBufferedImage
import it.codingjam.lagioconda.fitness.ByteComparisonFitness
import it.codingjam.lagioconda.ga.{MutationPointLike, _}
import it.codingjam.lagioconda.population.Individual
import it.codingjam.lagioconda.protocol.Messages.IndividualInfo
import it.codingjam.lagioconda.{Configuration, ImageDimensions, Population, WheelSelection}
import org.apache.commons.codec.binary.Base64OutputStream

import scala.util.Random

class PopulationActor(service: ActorSelection, out: ActorRef) extends Actor with ActorLogging {

  var state = Population(0, List[Individual](), 0, List())
  var generation = 0
  var n = 0
  var index = -1
  implicit val scheduler = context.system.scheduler

  val file = new File("frontend/public/images/monalisasmall2.png")

  val reference = ImageIO.read(file)

  val convertedImg: BufferedImage = new BufferedImage(reference.getWidth(), reference.getHeight(), BufferedImage.TYPE_3BYTE_BGR);
  convertedImg.getGraphics().drawImage(reference, 0, 0, null)

  implicit val configuration = Configuration(alpha = 128, length = Gene.Size)

  implicit val dimension = ImageDimensions(reference.getWidth, reference.getHeight)
  implicit val crossover = new RandomCrossoverPoint
  implicit var mutation: MutationPointLike = new RandomMutationPoint
  implicit val selection = new WheelSelection
  implicit val fitness = new ByteComparisonFitness(convertedImg, dimension)

  var best: Option[Individual] = None
  var initialBest = 0.0
  implicit var temperature = Temperature(1.0)
  var last100results = List[Int]()

  override def receive: Receive = {
    case cmd: SetupPopulation =>
      state = Population.randomGeneration()
      index = cmd.index
      best = state.individuals.headOption
      best.foreach(updateUI(cmd.index, _, 0.0, state.generation, 0.0, 0))

      initialBest = state.individuals.head.fitness
      log.debug("Initial Best {}", initialBest)
      sender() ! PopulationGenerated(cmd.index, state.generation)

    case cmd: PopulationActor.RunAGeneration =>
      val oldBest: Option[Individual] = best
      implicit val ec = context.dispatcher

      state = state.nextGeneration(service)

      temperature = Temperature(((1.0 - state.individuals.head.fitness) / (1.0 - initialBest)))
      //log.debug("Temperature " + temperature)

      best = state.individuals.headOption
      best.foreach { b =>
        oldBest.foreach { old =>
          if (b.fitness > old.fitness) {
            updateUI(cmd.index, b, b.fitness - old.fitness, state.generation, old.fitness, 0)
            //log.debug("New best for population {}@{} is {}", cmd.index, state.generation, format(b.fitness))
          }
        }
      }

      val ob = best.get
      val size = ob.chromosome.genes.size
      var temp = state
      var newBest = temp.individuals.head

      if (state.lastResults.sum < 0.0001 && state.lastResults.length >= Population.MaxRotate) {

        var useHc = true

        while (useHc) {
          val start = if (Random.nextInt(20) < 1) 0 else Math.max(0, size - 1)

          Range(start, size).map { gene =>
            var oldFitness = temp.bestIndividual.fitness
            temp = temp.hillClimb(service, ec, temperature, gene)
            if (temp.bestIndividual.fitness > oldFitness) {
              val newFitness = temp.bestIndividual.fitness
              updateUI(cmd.index, temp.bestIndividual, newFitness - oldFitness, state.generation, oldFitness, 0)
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
        temp = temp.addGene(service)
        updateUI(cmd.index, temp.bestIndividual, 0, state.generation, fBeforeAdd, 0)
        state = temp
        best = state.individuals.headOption
      }

      sender() ! GenerationRan(cmd.index, state.generation)

    case cmd: Migrate =>
      val l = Range(0, Population.EliteCount).map(_ => state.randomElite).toList
      cmd.otherPopulation ! Migration(cmd.index, l, cmd.otherPopulationIndex)
      sender ! MigrationDone(index)

    case cmd: Migration =>
      val oldBest = best
      state = state.doMigration(cmd.list, service, context.dispatcher, cmd.otherPopulationIndex)
      best = state.individuals.headOption
      best.foreach { b =>
        oldBest.foreach { old =>
          if (b.fitness > old.fitness) {
            updateUI(cmd.index, b, b.fitness - old.fitness, state.generation, old.fitness, cmd.otherPopulationIndex)
          }
        }
      }

  }

  private def format(d: Double) = f"$d%1.3f"

  def updateUI(population: Int, b: Individual, increment: Double, generation: Int, oldfitness: Double, otherPopulationIndex: Int)(
      implicit configuration: Configuration): Unit = {

    def format2(d: Double) = f"$d%1.5f"

    val bi = b.chromosome.toBufferedImage()

    val os = new ByteArrayOutputStream()
    val b64 = new Base64OutputStream(os)

    ImageIO.write(bi, "png", b64)
    val image = os.toString("UTF-8")

    val s =
      s"Fit: ${format(b.fitness * 100)}%, g: ${state.generation}, reason ${b.generatedBy}, genes: ${state.bestIndividual.chromosome.genes.length}"
    log.debug(
      "Population {}, reason {}, old fitness {}, increment {}",
      population + "/" + generation,
      b.generatedBy,
      format2(oldfitness * 100),
      format2(increment * 100)
    )

    out ! IndividualInfo(generation = generation, image = image, population = index, info = s)
  }
}

object PopulationActor {

  def props(service: ActorSelection, out: ActorRef): Props = Props(new PopulationActor(service, out))

  case class SetupPopulation(index: Int)

  case class Mutate(index: Int)

  case class Crossover(index: Int)

  case class RemoveWeaker(index: Int)

  case class RunAGeneration(index: Int)

  case class Migrate(index: Int, number: Int, otherPopulation: ActorRef, otherPopulationIndex: Int)

  case class Migration(index: Int, list: List[Individual], otherPopulationIndex: Int)

  case class MigrationDone(index: Int)

}
