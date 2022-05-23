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
  out ! "\n"
  out ! "\n"
  // para cerrar el fichero
  contenido.close



}
object PonBlancoActor {
  def props(fe: String, out: ActorRef) = Props(new PonBlancoActor(fe, out))
}

class PonFlechaActor (out: ActorRef) extends Actor{
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
object PonFlechaActor {
  def props(out: ActorRef) = Props(new PonFlechaActor(out))
}
class PonCambioDeLineaActor (fs: String) extends Actor{
  var contador = 0
  var resultado=""
  var primero=true
  def receive = {


    case msg =>
      // PrintWriter
      import java.io._

      val fo = new PrintWriter (new File(fs)) // fs es el nombre del fichero

      // para cerrar el fichero

        if(contador %14!=13){
          resultado=resultado+msg.toString
          print(msg)


        }else{
          resultado=resultado+"\n"+msg.toString
          print("\n"+msg)



      }


      val pw = new PrintWriter(new File(fs ))
      pw.write(resultado)
      pw.close
      contador =contador +1

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