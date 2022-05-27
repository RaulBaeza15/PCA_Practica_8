import ExeActors802.args
import akka.actor._

import scala.io._
import java.io._


class ActorQueGeneraAleatorios(n: Int,limite:Int, out: ActorRef) extends Actor{
  def receive = PartialFunction.empty
  val r = scala.util.Random
  for(i <- 0 to n){
    out ! r.nextInt(limite)
  }
  out ! -1
}

object ActorQueGeneraAleatorios{
  def props(n: Int,limite:Int, out: ActorRef)= Props(new ActorQueGeneraAleatorios(n,limite,out))
}

class ActorProcesin(siguiente:ActorRef) extends Actor{
  var numeroMenor =0
  var primero = true
  def receive ={
    case s: Int=>
      if(primero){
        numeroMenor=s
        primero = false
      }
      else{
        if (s == -1){
          print(numeroMenor+  ", ")
          siguiente ! s
        }else if(s<numeroMenor){
          siguiente ! numeroMenor
          numeroMenor = s
        }
        else{
          siguiente ! s
        }
      }
  }
}

object ActorProcesin{
  def props(siguiente: ActorRef)= Props(new ActorProcesin(siguiente))
}

class ActorUltimo() extends Actor{
  var numeroMenor =0
  var primero = true
  def receive ={
    case s: Int=>

      if(primero){
        numeroMenor=s
        primero = false
      }
      else{
        if (s == -1){
          print(numeroMenor)

        }
      }
  }
}

object ActorUltimo{
  def props()= Props(new ActorUltimo())
}

object ExeActorsE803Eliot extends App {
    val ourSystem = ActorSystem("RayadaQuftfteFlipas")
    val ultimisimo: ActorRef = ourSystem.actorOf(ActorUltimo.props())
  var n = args(0).toInt
  var limite = args(1).toInt
  var primero = true
  val procesos = new Array [ActorRef](n) //se indexan de 0 a n-1
  for(i <- 0 to n-1){
    if (primero){
      procesos(i) = ourSystem.actorOf(ActorProcesin.props(ultimisimo))
      primero = false
    }else{
      procesos(i) = ourSystem.actorOf(ActorProcesin.props(procesos(i-1)))
    }

  }
    val primerisimo: ActorRef = ourSystem.actorOf(ActorQueGeneraAleatorios.props(n,limite,procesos(n-1)))

    ourSystem.terminate()
    Thread.sleep (5000)
    println ("\nFIN")
}
