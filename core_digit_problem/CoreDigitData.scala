import scala.annotation.tailrec
import scala.io.Source

object CoreDigitData {

  def main(args: Array[String]): Unit = {

    if (args.length != 1) {
      println("Please provide an input core digit data file to process")
      sys.exit()
    }
    println(s"File to process: ${args(0)}")

    val source = Source.fromFile(args(0))
    val lines = source.getLines().toList
    source.close()

    lines.foreach(line => {
      val input:Array[String] = line.split("\\s")
      // val repeatedNo = repeatedNumber(input(0), input(1).toInt)
      val a = coreDigitCalculator(BigInt(input(0)))
      val b = coreDigitCalculator(BigInt(input(1)))
      // println(a + "-" + b)
      println(a * b)
    })
    // println(repeatedNumber("45", 3))
    // println(coreDigitCalculator(49458))
  }

  private def repeatedNumber(number: String, repeat: Int): String = {
    if (repeat == 1) number
    else number + repeatedNumber(number, repeat - 1)
  }

  @tailrec
  private def coreDigitCalculator(inputData: BigInt): Int = {
    @tailrec
    def coreDigitCalculatorAux(input: BigInt, acc: BigInt = 0): BigInt = {
      if (input <= 0) return acc
      else {
        coreDigitCalculatorAux(input/10, (acc + (input % 10)).toInt)
      }
    }

    val computedValue = coreDigitCalculatorAux(inputData)
    if (computedValue > 9)
      coreDigitCalculator(computedValue)
    else
      computedValue.toInt
  }
}
