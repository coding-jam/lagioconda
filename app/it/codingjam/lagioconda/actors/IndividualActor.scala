package it.codingjam.lagioconda.actors

import java.io.ByteArrayOutputStream
import javax.imageio.ImageIO

import akka.actor.{Actor, ActorRef, Props}
import it.codingjam.lagioconda.actors.IndividualActor._
import it.codingjam.lagioconda.conversions._
import it.codingjam.lagioconda.domain.ImageDimensions
import it.codingjam.lagioconda.ga.{RandomCrossoverPoint, Chromosome}
import it.codingjam.lagioconda.protocol.Messages.Individual
import org.apache.commons.codec.binary.Base64OutputStream

class IndividualActor(chromosome: Chromosome)(implicit dimensions: ImageDimensions) extends Actor {

  implicit val crossover = new RandomCrossoverPoint

  var state = chromosome
  override def receive: Receive = {

    case cmd: SendImage =>
      val bi = chromosome.toBufferedImage()
      val os = new ByteArrayOutputStream();
      val b64 = new Base64OutputStream(os);

      ImageIO.write(bi, "png", b64);
      val image = os.toString("UTF-8");
      cmd.out ! Individual(0, image)

    case cmd: Crossover =>
      cmd.withIndividual ! DoCrossover(state, cmd.replyTo)

    case Mutate =>
      state = state.mutate
      sender ! Mutation(state)

    case cmd: DoCrossover =>
      val newChromosomes: (Chromosome, Chromosome) =
        state.fullCrossover(cmd.chromosome)
      cmd.replyTo ! NewIndividual(newChromosomes._1)
      cmd.replyTo ! NewIndividual(newChromosomes._2)

  }
}

object IndividualActor {

  def props(chromosome: Chromosome)(implicit dimensions: ImageDimensions) =
    Props(new IndividualActor(chromosome))

  case class SendImage(out: ActorRef)

  case object Mutate

  case class Mutation(chromosome: Chromosome)

  case class Crossover(withIndividual: ActorRef, replyTo: ActorRef)

  case class DoCrossover(chromosome: Chromosome, replyTo: ActorRef)

  case class NewIndividual(chromosome: Chromosome)

}
