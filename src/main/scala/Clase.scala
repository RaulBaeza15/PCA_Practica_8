import scala.io._
import java.io._

import akka.actor._


class PonBlancoActor (fe: String, out: ActorRef) extends Actor {
  def receive = PartialFunction.empty // no recibe mensajesS
  val contenido = Source.fromFile(fe) // siendo fe el nombre de fichero
  var contador=1


  for (c <- contenido) {
    if(contador%11!=0){
      out ! c
    }else{out ! " "}
    contador=contador+1
  }
  // para cerrar el fichero
  contenido.close



}
object PonBlancoActor {
  def props(fe: String, out: ActorRef) = Props(new PonBlancoActor(fe, out))
}

class PonFlechaActor (out: ActorRef) extends Actor{
  var anterior=""
  def receive = {

    case msg =>
      if(anterior==""){


      }
      print(msg)

  }


}
object PonFlechaActor {
  def props(out: ActorRef) = Props(new PonFlechaActor(out))
}
class PonCambioDeLineaActor (fs: String) extends Actor{
  def receive = {
    case "hi" =>
      print("Mamasita\n")
    case msg =>
      print("Irene\n")
      context.stop(self)
  }

}
object PonCambioDeLineaActor {
  def props(fs: String) = Props(new PonCambioDeLineaActor(fs))
}


object ExeActors801 extends App {
  val fe = "in.txt"
  val fs = "out"
  val ourSystem = ActorSystem("TransformaCaracteres")
  val pCL: ActorRef = ourSystem.actorOf(PonCambioDeLineaActor.props(fs))
  val pFl: ActorRef = ourSystem.actorOf (PonFlechaActor.props (pCL))
  val pBl: ActorRef = ourSystem.actorOf (PonBlancoActor.props (fe, pFl))

  pBl !
  Thread.sleep (5000)
  println ("FIN")

  ourSystem.terminate
}