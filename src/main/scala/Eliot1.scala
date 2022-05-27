import akka.actor._
import scala.io._
import java.io._
class PonBlancoActorEliot (fe: String, out: ActorRef) extends Actor {
  var contador = 1;
  def receive = PartialFunction.empty // no recibe mensajes
  val contenido = Source.fromFile(fe) // siendo fe el nombre de fichero
  for (c <- contenido) {
    //iteración
    if(contador>0 && contador %11==0){
      out ! " "
    }
    else {
      out ! c
    }
    contador = contador + 1
  }
  // para cerrar el fichero
  contenido.close

}
object PonBlancoActorEliot {
  def props(fe: String, out: ActorRef) = Props(new PonBlancoActorEliot(fe, out))
}

class PonFlechaActorEliot(out: ActorRef) extends Actor{
  var primero=true
  var anterior: String = "_"
  def receive = {
    case msg =>
      if(primero){
        anterior=msg.toString
        primero=false
      }else{
        if(anterior==msg.toString&&anterior=="*"){
          out ! "^"
          anterior = ""
        }else{
          out ! anterior
          anterior=msg.toString
        }
      }
  }
}
object PonFlechaActorEliot {
  def props(out: ActorRef) = Props(new PonFlechaActorEliot(out))
}
class PonCambioDeLineaActorEliot (fs: String) extends Actor{
  var contador = 0
  var resultado=""
  var primero=true
  def receive = {
    case s =>
      // PrintWriter
      import java.io._
      val fo = new PrintWriter (new File(fs)) // fs es el nombre del fichero
      // para cerrar el fichero
      if(contador %14!=13){
        resultado=resultado+s.toString
        print(s)
      }else{
        resultado=resultado+"\n"+s.toString
        print("\n"+s)
      }
      val pw = new PrintWriter(new File(fs ))
      pw.write(resultado)
      pw.close
      contador =contador +1
  }
}
object PonCambioDeLineaActorEliot {
  def props(fs: String) = Props(new PonCambioDeLineaActorEliot(fs))
}
object ExeActorsE801Eliot extends App {
  val fe = "in.txt"
  val fs = "out"
  val ourSystem = ActorSystem("TransformaCaracteres")
  val pCL: ActorRef = ourSystem.actorOf(PonCambioDeLineaActorEliot.props(fs))
  val pFl: ActorRef = ourSystem.actorOf (PonFlechaActorEliot.props (pCL))
  val pBl: ActorRef = ourSystem.actorOf (PonBlancoActorEliot.props (fe, pFl))

  Thread.sleep (5000)
  println ("FIN")
  ourSystem.terminate
}

/**
LasManifes ac
iones^o tento
sas*S bre^Cos
as bsurdas
 */
