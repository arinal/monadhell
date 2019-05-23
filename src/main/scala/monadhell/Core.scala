package monadhell

import scalaz._, Scalaz._

object Core {

  sealed trait TempUnit
  case object Celsius    extends TempUnit
  case object Fahrenheit extends TempUnit

  case class Temperature(value: Int, unit: TempUnit = Celsius)
  case class Forecast(temperature: Temperature)
  case class City(name: String)

  sealed trait Error
  case class UnknownCity(city: String) extends Error

  def cityByName(cityName: String): Error \/ City = cityName match {
    case "Wroclaw" =>  \/-(City(cityName))
    case "London"  =>  \/-(City(cityName))
    case "Jakarta" =>  \/-(City(cityName))
    case _         => -\/ (UnknownCity(cityName))
  }

  type Requests = Map[City, Forecast]

  object Requests {

    implicit val order: Order[(City, Forecast)] =
      Order.order[(City, Forecast)](_._2.temperature.value cmp _._2.temperature.value)

    def empty: Requests                                       = Map.empty[City, Forecast]
    def hottest(requests: Requests): Option[(City, Forecast)] = requests.toList.maximum
  }

  def hottestCity: State[Requests, Option[(City, Temperature)]] = State { reqs =>
    val temper = Requests.hottest(reqs).map(f => (f._1, f._2.temperature))
    (reqs, temper)
  }
}
