package it.codingjam.lagioconda.actors

import java.awt.image.{BufferedImage, DataBufferByte}
import java.io.{ByteArrayOutputStream, File}
import javax.imageio.ImageIO

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSelection, Props}
import it.codingjam.ga.{Population, WheelSelection}
import it.codingjam.lagioconda.actors.PopulationActor.{Migrate, Migration, MigrationDone, SetupPopulation}
import it.codingjam.lagioconda.actors.SocketActor.{GenerationRan, PopulationGenerated}
import it.codingjam.lagioconda.domain.{Configuration, ImageDimensions}
import it.codingjam.lagioconda.fitness.ByteComparisonFitness
import it.codingjam.lagioconda.ga.{Gene, RandomCrossoverPoint, RandomMutationPoint, Temperature}
import it.codingjam.lagioconda.protocol.Messages.Individual
import org.apache.commons.codec.binary.Base64OutputStream
import it.codingjam.lagioconda.conversions.ChromosomeToBufferedImage
import it.codingjam.lagioconda.models.IndividualState

class PopulationActor(service: ActorSelection, out: ActorRef) extends Actor with ActorLogging {

  var state = Population(0, List[IndividualState](), 0.0, 0, "???", 0, 0.0, List(), 0)
  var generation = 0
  var n = 0
  var index = -1

  val file = new File("frontend/public/images/monalisasmall2.png")

  val reference = ImageIO.read(file)

  val convertedImg = new BufferedImage(reference.getWidth(), reference.getHeight(), BufferedImage.TYPE_3BYTE_BGR);
  convertedImg.getGraphics().drawImage(reference, 0, 0, null)

  val referenceInByte = convertedImg.getRaster().getDataBuffer().asInstanceOf[DataBufferByte].getData()
  implicit val configuration = Configuration(alpha = 128, length = Gene.Size)

  implicit val dimension = ImageDimensions(reference.getWidth, reference.getHeight)
  implicit val fitnessFunction = new ByteComparisonFitness(referenceInByte)
  implicit val crossover = new RandomCrossoverPoint
  implicit val mutation = new RandomMutationPoint
  implicit val selection = new WheelSelection

  var best: Option[IndividualState] = None
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
      log.debug("Initial Mean fitness {}", state.meanFitness)
      log.debug("Initial Best {}", initialBest)
      sender() ! PopulationGenerated(cmd.index, state.generation)

    case cmd: PopulationActor.RunAGeneration =>
      val oldFitness = state.meanFitness
      val oldBest = best

      state = state.nextGeneration(service, context.dispatcher)

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

  private def compareFitnesses(a: Double, b: Double) = {
    if (a < b) "+"
    else if (a > b) "-"
    else "="
  }

  private def format(d: Double) = f"$d%1.3f"

  def updateUI(population: Int, b: IndividualState, increment: Double, generation: Int, oldfitness: Double, otherPopulationIndex: Int)(
      implicit configuration: Configuration): Unit = {

    def format2(d: Double) = f"$d%1.5f"

    val bi = b.chromosome.toBufferedImage()

    val os = new ByteArrayOutputStream()
    val b64 = new Base64OutputStream(os)

    ImageIO.write(bi, "png", b64)
    val image = os.toString("UTF-8")

    val s = s"Fit: ${format(b.fitness * 100)}%, g: ${state.generation}, reason ${state.bestReason}"
    log.debug(
      "Population {}, reason {}, old fitness {}, increment {}",
      population + "/" + generation,
      state.bestReason,
      format2(oldfitness * 100),
      format2(increment * 100)
    )

    out ! Individual(generation = generation, image = image, population = index, info = s)
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

  case class Migration(index: Int, list: List[IndividualState], otherPopulationIndex: Int)

  case class MigrationDone(index: Int)

}
