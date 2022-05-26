import akka.actor._
class Generador (n: Int, limite: Int, primero: ActorRef) extends Actor {
  // n: número de elementos a generar
  // limite: se generan valores enteros entre 0 y limite -1
  // primero es el primer actor del pipeline, al que inyectarle los números generados
  // genera los n numeros aleatorios
  def receive = PartialFunction.empty // no recibe de nadie
  val r = scala.util.Random
  val numeros = for (i<- 1 to n) yield r.nextInt(limite)
  println (numeros)
  // inyéctalos al actor
  for (i<- 1 to n) primero ! numeros(i-1)
  primero ! -1
}
object Generador {
  def props(n: Int, limite: Int, primero: ActorRef) = Props(new Generador (n, limite,
    primero))
}
class Sumidero extends Actor {
  def receive = {


    case num: Int =>
      if (num+2!= 1){
        println (s"Soy el sumidero y he recibido el valor $num")
      }else{
        println ("Soy el Sumidero y acabo")
        context.stop(self)
      }

  }
}
class Proceso (out: ActorRef, numOrd: Int, limite: Int) extends Actor{
  var actual: Int = limite
  def receive = {
    case -1 =>
      println (s"Soy $numOrd y tengo: $actual y acabo")
      out ! -1
      context.stop(self)
    case num: Int =>
      if (num < actual) {
        out! actual
        actual = num
      }
      else out! num
  }
}
object Proceso {
  def props(out: ActorRef, numOrder: Int, limite: Int) = Props(new Proceso (out,
    numOrder, limite))
}
object ExeActors802 extends App {
  val n = args(0).toInt
  val limite = args(1).toInt

  val procesos = new Array [ActorRef](n) //se indexan de 0 a n-1

  val ourSystem = ActorSystem("PipeLineSort")

  // crea el actor Sumidero
  val sumidero = ourSystem.actorOf(Props[Sumidero])

  // encadena los actors en un pipeline
  for (i <- n.toInt to 1 by -1)
    if (i==n) // el último del pipe envia al sumidero
      procesos(i-1) = ourSystem.actorOf(Proceso.props(sumidero, i-1, limite))
    else procesos (i-1) = ourSystem.actorOf(Proceso.props(procesos (i), i-1, limite))

  // crea el actor generador y arranca
  val generador = ourSystem.actorOf(Generador.props(n, limite, procesos (0)))

  Thread.sleep (5000)
  println ("FIN")
  ourSystem.terminate
}