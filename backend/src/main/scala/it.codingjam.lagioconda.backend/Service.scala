package it.codingjam.lagioconda.backend

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import akka.actor._
import it.codingjam.lagioconda.ImageDimensions
import it.codingjam.lagioconda.fitness.ByteComparisonFitness
import it.codingjam.lagioconda.protocol.Message.{CalculateFitness, CalculatedFitness}

class WorkerActor extends Actor with ActorLogging {

  private val file = new File("src/main/resources/monalisa.png")

  private val reference = ImageIO.read(file)
  private implicit val dimension = ImageDimensions(reference.getWidth, reference.getHeight)

  private val convertedImg = new BufferedImage(reference.getWidth(), reference.getHeight(), BufferedImage.TYPE_3BYTE_BGR);
  convertedImg.getGraphics().drawImage(reference, 0, 0, null)

  private implicit val fitnessFunction = new ByteComparisonFitness(convertedImg, dimension)

  def receive = {
    case message: CalculateFitness =>
      sender() ! CalculatedFitness(message.chromosome, message.reason, fitnessFunction.fitness(message.chromosome, message.alpha))
    case x =>
      log.error("Undefined command {}", x)
  }

}

object ServiceBackend {

  def startOn(system: ActorSystem) { system.actorOf(Props[WorkerActor], name = "fitnessBackend") }

}
