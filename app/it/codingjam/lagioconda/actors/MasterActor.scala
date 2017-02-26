package it.codingjam.lagioconda.actors

import java.io.File

import akka.actor.{PoisonPill, Actor, ActorRef, Props}
import it.codingjam.lagioconda.actors.IndividualActor.{
  Crossover,
  Mutation,
  NewIndividual,
  SendImage
}
import it.codingjam.lagioconda.actors.MasterActor.SetupPopulation
import it.codingjam.lagioconda.actors.SocketActor.PopulationGenerated
import it.codingjam.lagioconda.conversions._
import it.codingjam.lagioconda.domain.ImageDimensions
import it.codingjam.lagioconda.fitness.HistogramFitness
import it.codingjam.lagioconda.ga.{Chromosome, RandomChromosome}
import org.bytedeco.javacpp.opencv_imgcodecs._

class MasterActor(out: ActorRef) extends Actor {

  var state = MasterState(List[IndividualState]())
  var generated = 0
  var n = 0

  val file = new File("resources/monalisa.jpg")
  println("*** " + file.getAbsolutePath)
  val reference = imread(file.getAbsolutePath, IMREAD_COLOR)
  println("IC " + reference.channels)
  var comparator = new HistogramFitness(reference)
  implicit val dimension = ImageDimensions(reference.rows(), reference.cols())

  var best: Option[IndividualState] = None

  override def receive: Receive = {
    case a @ SetupPopulation =>
      Range(0, 500).foreach { i =>
        val c: Chromosome = RandomChromosome.generate()
        val child =
          context.actorOf(IndividualActor.props(c), name = "individual" + i)
        val im = c.toBufferedImage
        val mat = im.toMat
        val fitness = comparator.fitness(mat)
        mat.release()
        mat._deallocate()

        val individual = IndividualState(child, fitness)
        state = state.addIndividual(individual).reverseSort
        val oldBest = best
        best = state.population.headOption
        if (oldBest != best) {
          best.foreach(b => b.actorRef ! SendImage(out))
        }
      }
      generated = 500

      println(f"Initial Mean fitness: ${state.meanFitness}%1.6f")
      sender() ! PopulationGenerated

    case MasterActor.Mutate =>
      state.randomIndividual.actorRef ! IndividualActor.Mutate

    case msg: Mutation =>
      val im = msg.chromosome.toBufferedImage
      val fitness = comparator.fitness(im.toMat)
      state.removeIndividual(sender())
      val individual = IndividualState(sender(), fitness)
      state.addIndividual(individual)
      state = state.addIndividual(individual).reverseSort

      // new best
      best.foreach { b =>
        if (state.population.head.fitness > b.fitness) {
          best = state.population.headOption
          best.foreach(b => b.actorRef ! SendImage(out))
        }
      }
      n = n + 1
      if (n % 50 == 0)
        println(f"Mean fitness: ${state.meanFitness}%1.6f")

    case MasterActor.Crossover =>
      val first = state.randomIndividual.actorRef
      val second = state.randomIndividual.actorRef
      first ! Crossover(second, self)

    case msg: NewIndividual =>
      val im = msg.chromosome.toBufferedImage
      val mat = im.toMat
      val fitness = comparator.fitness(mat)
      mat.release()
      mat._deallocate()

      generated = generated + 1
      val child =
        context.actorOf(IndividualActor.props(msg.chromosome),
                        name = "individual" + generated)

      val individual = IndividualState(child, fitness)
      state.addIndividual(individual)
      state = state.addIndividual(individual).reverseSort

      // new best
      best.foreach { b =>
        if (state.population.head.fitness > b.fitness) {

          best = state.population.headOption
          best.foreach { b =>
            b.actorRef ! SendImage(out)
            println(f"!!!new best!!! ${b.fitness}%1.6f")
          }
        }
      }
      n = n + 1
      if (n % 10 == 0)
        println(f"Mean fitness: ${state.meanFitness}%1.6f")

    case MasterActor.RemoveWeaker =>
      val initialSize = state.population.size
      val factor = state.population.size - 1000
      if (factor > 0) {
        val l = state.population.splitAt(1000)
        state = MasterState(l._1)
        l._2.foreach(is => is.actorRef ! PoisonPill)
      }
      val m = state.population.partition(_.fitness > 0)
      state = MasterState(m._1)
      m._2.foreach(is => is.actorRef ! PoisonPill)
    //println("Purged " + (state.population.size - initialSize))
  }
}

object MasterActor {

  def props(out: ActorRef) = Props(new MasterActor(out))

  case object SetupPopulation

  case object Mutate

  case object Crossover

  case object RemoveWeaker

}
