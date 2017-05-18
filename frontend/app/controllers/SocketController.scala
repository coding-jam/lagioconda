package it.codingjam.lagioconda.controllers

import akka.actor.ActorSystem
import akka.stream.Materializer
import com.google.inject.Inject
import it.codingjam.lagioconda.actors.SocketActor
import play.api.libs.streams.ActorFlow
import play.api.mvc.{Controller, WebSocket}
import protocol.Messages.messageFlowTransformer
import protocol.{InEvent, OutEvent}

class SocketController @Inject()(implicit system: ActorSystem, materializer: Materializer) extends Controller {

  def socket = WebSocket.accept[InEvent, OutEvent] { request =>
    ActorFlow.actorRef(out => SocketActor.props(out))
  }

}
