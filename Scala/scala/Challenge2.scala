object Challenge2 {
  def main(Args: Array[String]): Unit = {
    // Write a function called product which takes two parameters of type int and multiply them together
    def product(parum: Int, parum2: Int, parum3: Int): Int = parum * parum2 * parum3;
    println(product(3,29, 19))

    /*
    Write a function called greeting, which takes a parameter called Name of type String and outputs
    a String; the parameter should have a default value, so that an appropriate String is outputted if the
    function is called without a parameter.
     */
    println(greetingPerson(greets = false))
    println(greetingPerson(greets = true))
    def greeting(n1: String): String = "David";
    def greetingPerson(greets: Boolean = false): String = if (greets) "Hello David, How is your day?"
                                                          else "You no welcome.... >;["
  }
}
