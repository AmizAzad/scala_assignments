import scala.io.Source

object Thiefdata {

  def main(args: Array[String]): Unit = {

    if (args.length != 1) {
      println("Please provide an input thief data csv file to process")
      sys.exit()
    }
    println(s"File to process: ${args(0)}")

    Source.fromFile(args(0)).getLines().toList.foreach(line => {
      println(remoteUsage(line))
    })
    // println(remoteUsage("0110101001000010110100001011001000101110010011001"))
  }

  private def remoteUsage(input: String): Int = {
    if (input.isEmpty || input.isBlank) return 0

    var remoteUsage = 0

    for (i <- 1 until input.length) {
      if (input(i) != input(i-1)) {
        remoteUsage += 1
      }
    }

    if (input(0) == '0') remoteUsage + 1 else remoteUsage
  }

}
