import scala.io._
import java.io._
import akka.actor._

class ActorAnd (out: ActorRef) extends Actor{
  var num =0
  var primero=true
  def receive = {
    case int: Int =>
      if(primero){
        num =int
        primero=false
      }else{
        out ! num * int
      }

  }
}
object ActorAnd {
  def props(out: ActorRef) = Props(new ActorAnd(out))
}

class ActorOr () extends Actor{
  var num =0
  var primero=true
  def receive = {
    case int: Int =>
      if(primero){
        num =int
        primero=false
      }else{
        if(num+int==0){
          print("0\n")
        }else{
          print("1\n")
        }

      }

  }

}
object ActorOr {
  def props() = Props(new ActorOr())
}


object ExeActors803 extends App {

  for (a <- 0 to 1 ){

    for (b <- 0 to 1 ){
      for (c <- 0 to 1 ){
        val ourSystem = ActorSystem("circuito")
        val A3: ActorRef = ourSystem.actorOf(ActorOr.props())
        val A2: ActorRef = ourSystem.actorOf(ActorAnd.props(A3))
        val A1: ActorRef = ourSystem.actorOf(ActorAnd.props(A3))
        print(a,b,c)
        print("\n")
        A1 ! a.toInt
        A1 ! b.toInt
        A2 ! b.toInt
        A2 ! c.toInt

        ourSystem.terminate()

      }



    }


  }
  Thread.sleep (5000)
  println ("FIN")


}