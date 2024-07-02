import scala.io.Source

object Examdata {
  def main(args: Array[String]): Unit = {

    if (args.length != 1) {
      println("Please provide an input exam data csv file to process")
      sys.exit()
    }
    println(s"File to process: ${args(0)}")

    Source.fromFile(args(0)).getLines().toList.foreach(line => {
      val element: Array[Int] = line.split(",").map(_.trim.toInt)
      val timeForPrep = element(0) * element(1)
      if (timeForPrep <= element(2)) println("YES") else println("NO")
    })
  }
}