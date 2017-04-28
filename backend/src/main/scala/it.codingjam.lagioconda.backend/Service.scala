package it.codingjam.lagioconda.backend

import java.awt.image.{BufferedImage, DataBufferByte}
import java.io.File
import javax.imageio.ImageIO

import akka.actor._
import it.codingjam.ga.protocol.Messages.{CalculateFitness, CalculatedFitness}
import it.codingjam.lagioconda.backend.domain.{ByteComparisonFitness, Configuration, ImageDimensions}

class WorkerActor extends Actor with ActorLogging {

  println("this is " + self.path)

  val file = new File("src/main/resources/monalisasmall2.png")

  println("file " + file.getAbsolutePath)
  val reference = ImageIO.read(file)
  implicit val dimension = ImageDimensions(reference.getWidth, reference.getHeight)

  val convertedImg = new BufferedImage(reference.getWidth(), reference.getHeight(), BufferedImage.TYPE_3BYTE_BGR);
  convertedImg.getGraphics().drawImage(reference, 0, 0, null)

  val referenceInByte = convertedImg.getRaster().getDataBuffer().asInstanceOf[DataBufferByte].getData()
  implicit val configuration = Configuration(alpha = 64, length = 47)

  implicit val fitnessFunction = new ByteComparisonFitness(referenceInByte)

  def receive = {
    case message: CalculateFitness =>
      println("generation " + message.generation + " chromosome " + message.chromosome.hashCode())
      sender() ! CalculatedFitness(message.chromosome, message.reason, fitnessFunction.fitness(message.chromosome))
    case x =>
      log.error("Undefined command {}", x)

  }

}

object ServiceBackend {

  def startOn(system: ActorSystem) { system.actorOf(Props[WorkerActor], name = "fitnessBackend") }

}
