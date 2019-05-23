package monadhell

import monix.eval.Task
import scalaz._, Scalaz._
import shims._

object Main {

  type Effect0[A]              = EitherT[Task, Error, A]
  type Effect[A]               = StateT[Effect0, Requests, A]
  type ErrorEitherT[M[_], A]   = EitherT[M, Error, A]
  type RequestsStateT[M[_], A] = StateT[M, Requests, A]

  implicit class EffectOps[A](val effect: Effect[Unit]) extends AnyVal {
    def forever: Effect[Unit] = effect.flatMap { _ => forever }
  }

  sealed trait TempUnit
  case object Celsius    extends TempUnit
  case object Fahrenheit extends TempUnit

  case class Temperature(value: Int, unit: TempUnit = Celsius)
  case class Forecast(temperature: Temperature)
  case class City(name: String)

  class WeatherClient(host: String, port: Int) {
    def forecast(city: City): Task[Forecast] = Task.delay {
      city match {
        case City("Wroclaw") => Forecast(Temperature(0))
        case City("London")  => Forecast(Temperature(8))
        case City("Jakarta") => Forecast(Temperature(38))
      }
    }
  }

  case class Config(host: String, port: Int)

  sealed trait Error
  case class UnknownCity(city: String) extends Error

  type Requests = Map[City, Forecast]

  object Requests {

    implicit val order: Order[(City, Forecast)] =
      Order.order[(City, Forecast)](_._2.temperature.value cmp _._2.temperature.value)

    def empty: Requests                                       = Map.empty[City, Forecast]
    def hottest(requests: Requests): Option[(City, Forecast)] = requests.toList.maximum
  }

  def printLn(line: String): Task[Unit] = Task.delay(println(line))
  def readLn: Task[String]              = Task.delay(scala.io.StdIn.readLine)

  def cityByName(cityName: String): Error \/ City = cityName match {
    case "Wroclaw" => \/-(City(cityName))
    case "London" => \/-(City(cityName))
    case "Jakarta" => \/-(City(cityName))
    case _ => -\/(UnknownCity(cityName))
  }

  def hottestCity: State[Requests, Option[(City, Temperature)]] = State { reqs =>
    val temper = Requests.hottest(reqs).map(f => (f._1, f._2.temperature))
    (reqs, temper)
  }

  def weatherTask(city: City)(host: String, port: Int): Task[Forecast] =
    new WeatherClient(host, port).forecast(city)

  def askCityTask: Task[String] = for {
    _    <- printLn("What is the next city?")
    name <- readLn
  } yield name

  def fetchForecast(city: City)(host: String, port: Int): RequestsStateT[Task, Forecast] =
    for {
      mForecast <- StateT { reqs: Requests =>
                       Task.delay((reqs, reqs.get(city)))
                     }
      forecast  <- StateT { reqs: Requests =>
                       mForecast.cata(_.pure[Task], weatherTask(city)(host, port)).map(f => (reqs, f))
                     }
      _         <- StateT { reqs: Requests =>
                       Task.delay((reqs + (city -> forecast), ()))
                     }
    } yield forecast

  val askFetchJudge: Effect[Unit] = for {
      cityName <- askCityTask
                    .liftM[ErrorEitherT]
                    .liftM[RequestsStateT]
      city     <- EitherT.fromDisjunction[Task](cityByName(cityName))
                    .liftM[RequestsStateT]
      forecast <- fetchForecast(city)("http://localhost", 80)
                    .mapK[Effect0, Forecast, Requests](t => EitherT(t.map(_.right[Error])))
      _        <- printLn(s"Forecast for ${city.name} is ${forecast.temperature}")
                    .liftM[ErrorEitherT].liftM[RequestsStateT]
      hottest  <- hottestCity.mapK[Effect0, Option[(City, Temperature)], Requests]{ a =>
                    EitherT(Task(a).map(_.right[Error]))
                  }(scalaz.idInstance)
      message  = hottest match {
                   case Some((City(name), Temperature(n, _))) => s"Hottest city found so far is $name with temperature $n"
                   case _ => "Couldn't found the hottest city"
                 }
      _        <- printLn(message)
                    .liftM[ErrorEitherT]
                    .liftM[RequestsStateT]
    } yield ()

  val program: Effect[Unit] = for {
    _ <- askFetchJudge.forever
  } yield ()

  def main(args: Array[String]): Unit = {

    import monix.execution.Scheduler.Implicits.global

    (program.run(Requests.empty).run >>= {
       case -\/(error) => printLn(s"Encountered an error: $error")
       case \/-(_)     => ().pure[Task]
     }).runSyncUnsafe()
  }
}
