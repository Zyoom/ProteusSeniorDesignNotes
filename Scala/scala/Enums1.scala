package Chapter4

@main def hello(): Unit = println()

object Example {
  enum TrafficLight:
    case Red, Amber, Green

    // Importing all members of TrafficLight to avoid specifying TrafficLight.*
    import TrafficLight.*

    // Define drivingSignal Functions
    def drivingSignal(light: TrafficLight): String =
      if light == Red then "STOP"
      else if light == Amber then "SLOW DOWN"
      else "GO"

    def drivingSignal2(light: TrafficLight): String = light match {
      case Red => "STOP"
      case Amber => "SLOW DOWN"
      case Green => "GO"
    }

  enum Triathlon(val metres: Int):
    case Swim extends Triathlon(400)
    case Cycle extends Triathlon(5000)
    case Run extends Triathlon(2500)

  import Triathlon.*

  def howManyMetres(tri: Triathlon): Int = tri match {
    case Swim => Swim.metres
    case Cycle => Cycle.metres
    case Run => Run.metres
  }
}
