object FizzBuzz {
  def fizzBuzz(max: Int): Seq[String] = {
    (1 to max).map {
      case i if (i % 15 == 0) => "FizzBuzz"
      case i if (i % 3 == 0) => "Fizz"
      case i if (i % 5 == 0) => "Buzz"
      case i => i.toString
    }
  }
}
