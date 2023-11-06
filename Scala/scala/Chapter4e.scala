package Chapter4E

import Chapter4E.Example.UnitedKingdom

object Example {
  case class Country(name: String, population: Int)

  def population(country: Country): Int = country.population

  enum UnitedKingdom:
    case NorthernIreland, England, Scotland, Wales

  import UnitedKingdom._

  val country = England
  if (country == Scotland) {
    println("Welcome To Scotland")
  }

  def whereAmI(country: UnitedKingdom): String = country match {
    case England => "We're in England"
    case NorthernIreland => "We're in Northern Ireland"
    case Scotland => "We're in Scotland"
    case Wales => "We're in Wales"
  }
}

object Main {
  def chapter4E(): Unit = println(Example.whereAmI(UnitedKingdom.Scotland))

  def main(args: Array[String]): Unit = {
    chapter4E()
  }
}
