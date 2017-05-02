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
import it.codingjam.lagioconda.ga.{RandomCrossoverPoint, RandomMutationPoint, Temperature}
import it.codingjam.lagioconda.protocol.Messages.Individual
import org.apache.commons.codec.binary.Base64OutputStream
import it.codingjam.lagioconda.conversions.ChromosomeToBufferedImage
import it.codingjam.lagioconda.models.IndividualState

class PopulationActor(service: ActorSelection, out: ActorRef) extends Actor with ActorLogging {

  var state = Population(0, List[IndividualState](), 0.0, 0, "???", 0, 0.0)
  var generation = 0
  var n = 0
  var index = -1

  val file = new File("frontend/public/images/monalisasmall2.png")

  val reference = ImageIO.read(file)

  val convertedImg = new BufferedImage(reference.getWidth(), reference.getHeight(), BufferedImage.TYPE_3BYTE_BGR);
  convertedImg.getGraphics().drawImage(reference, 0, 0, null)

  val referenceInByte = convertedImg.getRaster().getDataBuffer().asInstanceOf[DataBufferByte].getData()
  implicit val configuration = Configuration(alpha = 80, length = 47)

  implicit val dimension = ImageDimensions(reference.getWidth, reference.getHeight)
  implicit val fitnessFunction = new ByteComparisonFitness(referenceInByte)
  implicit val crossover = new RandomCrossoverPoint
  implicit val mutation = new RandomMutationPoint
  implicit val selection = new WheelSelection

  var best: Option[IndividualState] = None
  var initialBest = 0.0
  implicit var temperature = Temperature(1.0)

  override def receive: Receive = {
    case cmd: SetupPopulation =>
      state = Population.randomGeneration()
      index = cmd.index
      best = state.individuals.headOption
      best.foreach(updateUI(_, 0.0, state.generation))

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
            updateUI(b, b.fitness - old.fitness, state.generation)
            //log.debug("New best for population {}@{} is {}", cmd.index, state.generation, format(b.fitness))
          }
        }
      }
      sender() ! GenerationRan(cmd.index, state.generation)

    case cmd: Migrate =>
      val l = List(state.randomNotElite)
      cmd.otherPopulation ! Migration(l)
      sender ! MigrationDone(index)

    case cmd: Migration =>
      val oldBest = best
      state = state.addIndividuals(cmd.list)
      //log.debug("Mean fitness after migration for population {}@{} is {}", index, state.generation, format(state.meanFitness))
      best = state.individuals.headOption
      best.foreach { b =>
        oldBest.foreach { old =>
          if (b.fitness > old.fitness) {
            updateUI(b, b.fitness - old.fitness, state.generation)
            //log.debug("New best after migration for population {}@{} is {}", index, state.generation, format(b.fitness))
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

  def updateUI(b: IndividualState, increment: Double, generation: Int)(implicit configuration: Configuration): Unit = {

    def format2(d: Double) = f"$d%1.5f"

    val bi = b.chromosome.toBufferedImage()

    val os = new ByteArrayOutputStream()
    val b64 = new Base64OutputStream(os)

    ImageIO.write(bi, "png", b64)
    val image = os.toString("UTF-8")

    val s = s"Fit: ${format(b.fitness * 100)}%, generation: ${state.generation}, reason ${state.bestReason}"
    log.debug("Generation {}, reason {}, increment {}", generation, state.bestReason, format2(increment * 100))

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

  case class Migrate(index: Int, number: Int, otherPopulation: ActorRef)

  case class Migration(list: List[IndividualState])

  case class MigrationDone(index: Int)

}