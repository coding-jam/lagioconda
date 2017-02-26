package it.codingjam.lagioconda.actors

import java.io.{ByteArrayOutputStream, File}
import javax.imageio.ImageIO

import akka.actor.{Actor, ActorRef, Props}
import it.codingjam.lagioconda.actors.PopulationActor.SetupPopulation
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

  val file = new File("resources/monalisa.jpg")
  val reference = imread(file.getAbsolutePath, IMREAD_COLOR)

  implicit val fitnessFunction = new HistogramFitness(reference)
  implicit val dimension = ImageDimensions(reference.cols(), reference.rows())
  implicit val crossover = new RandomCrossoverPoint

  var best: Option[IndividualState] = None

  override def receive: Receive = {
    case a @ SetupPopulation =>
      state = Population.randomGeneration()
      best = state.individuals.headOption
      best.foreach(updateUI(_))

      println(f"Initial Mean fitness: ${state.meanFitness}%1.6f")
      sender() ! PopulationGenerated

    case PopulationActor.RunAGeneration =>
      val oldBest = best
      state = state.runAGeneration
      println(f"Mean fitness: ${state.meanFitness}%1.6f")
      best = state.individuals.headOption
      best.foreach { b =>
        oldBest.foreach { old =>
          if (b.fitness > old.fitness) {
            updateUI(b)
            println(f"New best!!: ${b.fitness}%1.6f")
          }
        }
      }
      sender() ! GenerationRan
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

  case object SetupPopulation

  case object Mutate

  case object Crossover

  case object RemoveWeaker

  case object RunAGeneration

}
