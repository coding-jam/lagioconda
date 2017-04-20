package it.codingjam.lagioconda.backend

import akka.actor._
import com.typesafe.config.ConfigFactory
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import collection.JavaConversions._

object Backend extends App {

  // Simple cli parsing
  val port = args match {
    case Array() => "0"
    case Array(port) => port
    case args => throw new IllegalArgumentException(s"only ports. Args [ $args ] are invalid")
  }

  // System initialization
  val properties = Map(
    "akka.remote.netty.tcp.port" -> port
  )

  println("PORT " + port)

  val system = ActorSystem("application",
                           (ConfigFactory
                             .parseMap(properties))
                             .withFallback(ConfigFactory.load()))

  // Deploy actors and services
  ServiceBackend.startOn(system)

  Await.result(system.whenTerminated, Duration.Inf)
}
