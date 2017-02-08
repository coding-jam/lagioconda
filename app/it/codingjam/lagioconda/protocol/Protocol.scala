package it.codingjam.lagioconda.protocol

import play.api.libs.json._
import play.api.mvc.WebSocket.MessageFlowTransformer

trait InEvent
trait OutEvent
object Messages {

  implicit object Writes extends Writes[OutEvent] {
    override def writes(o: OutEvent): JsValue = {
      o match {
        case i: Individual => Individual.format.writes(i)
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

  case class Individual(generation: Int, image: String) extends OutEvent

  object Individual {
    implicit val format = Json.format[Individual]
  }

  implicit val messageFlowTransformer =
    MessageFlowTransformer.jsonMessageFlowTransformer[InEvent, OutEvent]

}
