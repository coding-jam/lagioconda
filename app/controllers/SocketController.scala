package controllers

import akka.actor.ActorSystem
import akka.stream.Materializer
import com.google.inject.Inject
import it.codingjam.lagioconda.actors.SocketActor
import it.codingjam.lagioconda.protocol.{InEvent, OutEvent}
import it.codingjam.lagioconda.services.ImageGenerator
import play.api.libs.streams.ActorFlow
import play.api.mvc.{Controller, WebSocket}
import it.codingjam.lagioconda.protocol.Messages.messageFlowTransformer

class SocketController @Inject()(imageGenerator: ImageGenerator)(implicit system: ActorSystem, materializer: Materializer)
    extends Controller {

  def socket = WebSocket.accept[InEvent, OutEvent] { request =>
    ActorFlow.actorRef(out => SocketActor.props(out, imageGenerator))
  }

}
