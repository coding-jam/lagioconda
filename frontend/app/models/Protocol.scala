package it.codingjam.lagioconda.protocol

import play.api.libs.json._
import play.api.mvc.WebSocket.MessageFlowTransformer

sealed trait InEvent

sealed trait OutEvent

trait JSFormatter {
  def identifier = getClass.getSimpleName.dropRight(1)
}

object Messages {

  implicit object Writes extends Writes[OutEvent] {
    override def writes(o: OutEvent): JsValue = {
      o match {
        case i: Individual => Individual.format.writes(i)
        case s: Statistics => Statistics.format.writes(s)
        case x => sys.error("type not found " + o)
      }
    }
  }

  implicit object Reads extends Reads[InEvent] {
    override def reads(json: JsValue): JsResult[InEvent] = {
      val msgType = (json \ "msg").as[String]
      msgType match {
        case "start" => Start.format.reads(json)
      }
    }
  }

  case class Start(cycles: Int) extends InEvent

  object Start {
    implicit val format = Json.format[Start]
  }

  case class Individual(generation: Int, image: String, msg: String = Individual.identifier, population: Int, info: String)
      extends OutEvent

  object Individual extends JSFormatter {
    implicit val format = Json.format[Individual]
  }

  case class Statistics(message: String, msg: String = Statistics.identifier) extends OutEvent

  object Statistics extends JSFormatter {
    implicit val format = Json.format[Statistics]
  }

  implicit val messageFlowTransformer =
    MessageFlowTransformer.jsonMessageFlowTransformer[InEvent, OutEvent]

}