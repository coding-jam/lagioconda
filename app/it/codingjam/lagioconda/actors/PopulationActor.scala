package it.codingjam.lagioconda.actors

import java.io.{ByteArrayOutputStream, File}
import javax.imageio.ImageIO

import akka.actor.{Actor, ActorRef, Props}
import it.codingjam.lagioconda.actors.PopulationActor.{
  MigrationDone,
  Migration,
  Migrate,
  SetupPopulation
}
import it.codingjam.lagioconda.actors.SocketActor.{
  GenerationRan,
  PopulationGenerated
}
import it.codingjam.lagioconda.conversions._
import it.codingjam.lagioconda.domain.ImageDimensions
import it.codingjam.lagioconda.fitness.HistogramFitness
import it.codingjam.lagioconda.ga.RandomCrossoverPoint
import it.codingjam.lagioconda.protocol.Messages.Individual
import org.apache.commons.codec.binary.Base64OutputStream
import org.bytedeco.javacpp.opencv_imgcodecs._

class PopulationActor(out: ActorRef) extends Actor {

  var state = Population(List[IndividualState]())
  var generated = 0
  var n = 0
  var index = -1

  val file = new File("resources/monalisa.jpg")
  val reference = imread(file.getAbsolutePath, IMREAD_COLOR)

  implicit val fitnessFunction = new HistogramFitness(reference)
  implicit val dimension = ImageDimensions(reference.cols(), reference.rows())
  implicit val crossover = new RandomCrossoverPoint

  var best: Option[IndividualState] = None

  override def receive: Receive = {
    case cmd: SetupPopulation =>
      state = Population.randomGeneration()
      index = cmd.index
      best = state.individuals.headOption
      best.foreach(updateUI(_))

      println(f"Initial Mean fitness: ${state.meanFitness}%1.6f")
      sender() ! PopulationGenerated(cmd.index)

    case cmd: PopulationActor.RunAGeneration =>
      val oldBest = best
      state = state.runAGeneration
      println(f"Mean fitness (${cmd.index}): ${state.meanFitness}%1.6f")
      best = state.individuals.headOption
      best.foreach { b =>
        oldBest.foreach { old =>
          if (b.fitness > old.fitness) {
            updateUI(b)
            println(f"New best (${cmd.index})!!: ${b.fitness}%1.6f")
          }
        }
      }
      sender() ! GenerationRan(cmd.index)

    case cmd: Migrate =>
      val l = Range(0, cmd.number).map(_ => state.randomIndividual).toList
      cmd.otherPopulation ! Migration(l)
      sender ! MigrationDone(index)

    case cmd: Migration =>
      val oldFitness = state.meanFitness
      val oldBest = best
      state = state.addIndividuals(cmd.list)
      println(
        f"Mean fitness after migration (${index}): ${state.meanFitness}%1.6f (was ${oldFitness}%1.6f)")
      best = state.individuals.headOption
      best.foreach { b =>
        oldBest.foreach { old =>
          if (b.fitness > old.fitness) {
            updateUI(b)
            println(
              f"New best after migration (${index})!!: ${b.fitness}%1.6f")
          }
        }
      }

  }

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
