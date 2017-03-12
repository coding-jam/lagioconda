package it.codingjam.lagioconda.actors

import java.awt.image.{BufferedImage, DataBufferByte, DataBufferInt}
import java.io.{ByteArrayOutputStream, File}
import javax.imageio.ImageIO

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import it.codingjam.lagioconda.actors.PopulationActor.{Migrate, Migration, MigrationDone, SetupPopulation}
import it.codingjam.lagioconda.actors.SocketActor.{GenerationRan, PopulationGenerated}
import it.codingjam.lagioconda.conversions._
import it.codingjam.lagioconda.domain.ImageDimensions
import it.codingjam.lagioconda.fitness.ByteComparisonFitness
import it.codingjam.lagioconda.ga.{RandomCrossoverPoint, RandomMutationPoint}
import it.codingjam.lagioconda.protocol.Messages.Individual
import org.apache.commons.codec.binary.Base64OutputStream
import org.bytedeco.javacpp.opencv_imgcodecs._

class PopulationActor(out: ActorRef) extends Actor with ActorLogging {

  var state = Population(0, List[IndividualState]())
  var generated = 0
  var n = 0
  var index = -1

  val file = new File("resources/monalisasmall.png")
  //val reference = imread(file.getAbsolutePath, IMREAD_COLOR)

  val reference = ImageIO.read(file)

  val convertedImg = new BufferedImage(reference.getWidth(), reference.getHeight(), BufferedImage.TYPE_4BYTE_ABGR);
  reference.getGraphics().drawImage(convertedImg, 0, 0, null)

  val referenceInByte = reference.getRaster().getDataBuffer().asInstanceOf[DataBufferByte].getData()

  implicit val alpha = 128
  implicit val dimension = ImageDimensions(reference.getWidth, reference.getHeight)
  implicit val fitnessFunction = new ByteComparisonFitness(referenceInByte)
  implicit val crossover = new RandomCrossoverPoint
  implicit val mutation = new RandomMutationPoint

  var best: Option[IndividualState] = None

  override def receive: Receive = {
    case cmd: SetupPopulation =>
      state = Population.randomGeneration()
      index = cmd.index
      best = state.individuals.headOption
      best.foreach(updateUI(_))

      log.debug("Initial Mean fitness {}", format(state.meanFitness))
      sender() ! PopulationGenerated(cmd.index, state.generation)

    case cmd: PopulationActor.RunAGeneration =>
      val oldFitness = state.meanFitness
      val oldBest = best
      state = state.runAGeneration
      log.debug("Mean fitness for population {}@{} is {}, {}",
                cmd.index,
                state.generation,
                format(state.meanFitness),
                compareFitnesses(oldFitness, state.meanFitness))
      best = state.individuals.headOption
      best.foreach { b =>
        oldBest.foreach { old =>
          if (b.fitness > old.fitness) {
            updateUI(b)
            log.debug("New best for population {}@{} is {}", cmd.index, state.generation, format(b.fitness))
          }
        }
      }
      sender() ! GenerationRan(cmd.index, state.generation)

    case cmd: Migrate =>
      val l = Range(0, cmd.number).map(_ => state.randomIndividual).toList
      cmd.otherPopulation ! Migration(l)
      sender ! MigrationDone(index)

    case cmd: Migration =>
      val oldFitness = state.meanFitness
      val oldBest = best
      state = state.addIndividuals(cmd.list)
      log.debug("Mean fitness after migration for population {}@{} is {}", index, state.generation, format(state.meanFitness))
      best = state.individuals.headOption
      best.foreach { b =>
        oldBest.foreach { old =>
          if (b.fitness > old.fitness) {
            updateUI(b)
            log.debug("New best after migration for population {}@{} is {}", index, state.generation, format(b.fitness))
          }
        }
      }

  }

  private def compareFitnesses(a: Double, b: Double) = {
    if (a < b) "+"
    else if (a > b) "-"
    else "="
  }

  private def format(d: Double) = f"$d%1.5f"

  def updateUI(b: IndividualState): Unit = {
    val bi = b.chromosome.toBufferedImage()
    val os = new ByteArrayOutputStream();
    val b64 = new Base64OutputStream(os);

    ImageIO.write(bi, "png", b64);
    val image = os.toString("UTF-8");
    out ! Individual(0, image)
  }
}

object PopulationActor {

  def props(out: ActorRef) = Props(new PopulationActor(out))

  case class SetupPopulation(index: Int)

  case class Mutate(index: Int)

  case class Crossover(index: Int)

  case class RemoveWeaker(index: Int)

  case class RunAGeneration(index: Int)

  case class Migrate(index: Int, number: Int, otherPopulation: ActorRef)

  case class Migration(list: List[IndividualState])

  case class MigrationDone(index: Int)

}
