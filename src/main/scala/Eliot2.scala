import akka.actor._
import scala.io._
import java.io._

class HazANDEliot (out: ActorRef) extends Actor {
  var primerOperando = 0
  var primero = true
  def receive = {
    case int: Int =>
      if(primero){
        primerOperando = int
        primero = false
      }
      else{
        out ! int * primerOperando
      }
  }
}

object HazANDEliot {
  def props(out: ActorRef) = Props(new HazANDEliot(out))
}

class HazOREliot (out: ActorRef) extends Actor {
  var primerOperando = 0
  var primero = true
  def receive = {
    case int: Int =>
      if(primero){
        primerOperando = int
        primero = false
      }
      else{
        if(primerOperando + int == 0){
          out ! 0
        }
        else{
          out ! 1
        }
      }
  }
}

object HazOREliot {
  def props(out: ActorRef) = Props(new HazOREliot(out))
}

class PonResultadoPorPantallaEliot () extends Actor {
  def receive = {
    case s =>
      print("\n")
      print(s)
  }
}

object PonResultadoPorPantallaEliot {
  def props() = Props(new PonResultadoPorPantallaEliot())
}

object ExeActorsE802Eliot extends App {
  for (a <- 0 to 1 ) {
    for (b <- 0 to 1) {
      for (c <- 0 to 1) {
        val ourSystem = ActorSystem("EstoLoQueHaceEsDosANDyLuegoUnOR")
        val pSalida: ActorRef = ourSystem.actorOf(PonResultadoPorPantallaEliot.props())
        val pOR: ActorRef = ourSystem.actorOf(HazOREliot.props(pSalida))
        val pAND1: ActorRef = ourSystem.actorOf(HazANDEliot.props(pOR))
        val pAND2: ActorRef = ourSystem.actorOf(HazANDEliot.props(pOR))
        print(a, b, c)
        print("\n")
        pAND1 ! a
        pAND1 ! b.toInt
        pAND2 ! b.toInt
        pAND2 ! c.toInt

        ourSystem.terminate()
      }
    }
  }
  Thread.sleep (5000)
  println ("\nFIN")
}
