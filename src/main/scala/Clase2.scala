import akka.actor._






object ExeActors802 extends App{


  val r = scala.util.Random
  print(for (i<- 1 to 10) yield r.nextInt(100))



}