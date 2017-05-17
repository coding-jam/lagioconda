package it.codingjam.lagioconda.backend

import java.awt.image.{BufferedImage, DataBufferByte}
import java.io.File
import javax.imageio.ImageIO

import akka.actor._
import it.codingjam.lagioconda.fitness.{ByteComparisonFitness, CIE2000Comparison}
import it.codingjam.lagioconda.ga.Gene
import it.codingjam.lagioconda.protocol.Message.{CalculateFitness, CalculatedFitness}
import it.codingjam.lagioconda.{Configuration, ImageDimensions}

class WorkerActor extends Actor with ActorLogging {

  println("this is " + self.path)

  val file = new File("src/main/resources/monalisasmall2.png")

  println("file " + file.getAbsolutePath)
  val reference = ImageIO.read(file)
  implicit val dimension = ImageDimensions(reference.getWidth, reference.getHeight)

  val convertedImg = new BufferedImage(reference.getWidth(), reference.getHeight(), BufferedImage.TYPE_3BYTE_BGR);
  convertedImg.getGraphics().drawImage(reference, 0, 0, null)

  implicit val configuration = Configuration(alpha = 128, length = Gene.Size)

  implicit val fitnessFunction = new ByteComparisonFitness(convertedImg, dimension)

  def receive = {
    case message: CalculateFitness =>
      //println("generation " + message.generation + " chromosome " + message.chromosome.hashCode())
      sender() ! CalculatedFitness(message.chromosome, message.reason, fitnessFunction.fitness(message.chromosome))
    case x =>
      log.error("Undefined command {}", x)

  }

}

object ServiceBackend {

  def startOn(system: ActorSystem) { system.actorOf(Props[WorkerActor], name = "fitnessBackend") }

}
