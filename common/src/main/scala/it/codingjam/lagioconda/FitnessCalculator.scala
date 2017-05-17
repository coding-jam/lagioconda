package it.codingjam.lagioconda

import akka.actor.{ActorSelection, _}
import akka.pattern.ask
import it.codingjam.lagioconda.fitness.FitnessFunction
import it.codingjam.lagioconda.ga.Chromosome
import it.codingjam.lagioconda.population.Individual
import it.codingjam.lagioconda.protocol.Message.{CalculateFitness, CalculatedFitness}

import scala.collection.immutable
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

case class FitnessCalculator(fitnessService: ActorSelection, fitnessFunction: FitnessFunction, dimension: ImageDimensions)
                            (implicit executionContext: ExecutionContext, scheduler: Scheduler) extends Retrying {

  implicit val timeout = akka.util.Timeout(300.seconds)



  def calculate(cList: List[(Chromosome, String)], generation:Int): List[Individual] = {
    val futures: immutable.Seq[Future[Individual]] = cList.map { chromosome =>
      (fitnessService ? CalculateFitness(chromosome._1, generation, chromosome._2)).mapTo[CalculatedFitness].map { cf =>
        Individual(cf.chromosome, cf.fitness, cf.reason, generation)
      }
    }

    val future: Future[immutable.Seq[Individual]] = Future.sequence(futures)

    val f = retry(future, 120.seconds, 5)

    Await.result(f, 120.seconds).toList
  }

}
