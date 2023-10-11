object Function {
  def main(args: Array[String]): Unit = {
    println(sum(2,2));
    println(computerSwitch(true))
    println(computerSwitch(false))
    def sum(n1: Int, n2: Int): Int = n1 + n2;
    def computerSwitch(switchOn: Boolean = false): String = if (switchOn)  "Welcome !"
                                                  else "Goodbye !" // already as false
  }

}
