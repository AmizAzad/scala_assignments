import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.io.Source

case class Harvest(gatherer: String, date: LocalDate, fruit: String, amount: Double)
case class Price(fruit: String, date: LocalDate, price: Double)

object HarvestData extends App {

  val harvestData = readHarvestData("harvest_problem/harvest.csv")
  val priceData = readPriceData("harvest_problem/price.csv")

  val monthlyHarvest = harvestData.groupBy(h => (h.date.getYear, h.date.getMonth, h.gatherer))
  val monthlyIncome = harvestData.groupBy(h => (h.date.getYear, h.date.getMonth, h.fruit))

  println("\nBest gatherer per month:")
  bestGathererPerMonth(monthlyHarvest).foreach { case (k, v) => println(s"$k -> $v") }

  println("\nBest gatherer per fruit:")
  bestGathererPerFruit(harvestData).foreach { case (k, v) => println(s"$k -> $v") }

  println("\nBest earning fruit: ")
  println(bestEarningFruitOverall(harvestData, priceData))

  println("\nBest earning fruit per month: ")
  bestEarningFruitPerMonth(monthlyIncome, priceData).foreach { case (k, v) => println(s"$k -> $v") }

  println("\nLeast earning fruit: ")
  println(leastProfitableFruitOverall(harvestData, priceData))

  println("\nLeast earning fruit per month: ")
  leastProfitableFruitPerMonth(monthlyIncome, priceData).foreach { case (k, v) => println(s"$k -> $v") }

  println("\nBest earnings generator: ")
  println(topEarningGathererOverall(harvestData, priceData))

  println("\nBest earnings generator per month: ")
  topEarningGathererPerMonth(monthlyHarvest, priceData).foreach { case (k, v) => println(s"$k -> $v") }

  def readHarvestData(filePath: String): List[Harvest] = {
    val dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
    Source.fromFile(filePath).getLines().drop(1).map(line => {
      val Array(gatherer, date, fruit, amount) = line.split(",")
      Harvest(gatherer, LocalDate.parse(date, dateFormatter), fruit, amount.toDouble)
    }).toList
  }

  def readPriceData(filePath: String): List[Price] = {
    val dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
    Source.fromFile(filePath).getLines().drop(1).map(line => {
      val Array(fruit, date, price) = line.split(",")
      Price(fruit, LocalDate.parse(date, dateFormatter), price.toDouble)
    }).toList
  }

  def bestGathererPerMonth(monthlyHarvest: Map[(Int, java.time.Month, String), List[Harvest]]): Map[(Int, java.time.Month), String] = {
    monthlyHarvest.groupBy {
      case ((year, month, gatherer), _) => (year, month)
    }.map {
      case ((year, month), data) =>
        val bestGatherer = data.maxBy {
          case (_, harvests) => harvests.map(_.amount).sum
        }
        ((year, month), bestGatherer._1._3)
    }
  }

  def bestGathererPerFruit(harvestData: List[Harvest]): Map[String, String] = {
    harvestData.groupBy(_.fruit).map {
      case (fruit, fruitData) =>
        val bestGatherer = fruitData.groupBy(_.gatherer).maxBy {
          case (_, gathererData) => gathererData.map(_.amount).sum
        }
        (fruit, bestGatherer._1)
    }
  }

  def bestEarningFruitOverall(harvestData: List[Harvest], priceData: List[Price]): String = {
    val fruitEarnings = harvestData.groupBy(_.fruit).map {
      case (fruit, fruitData) =>
        val earnings = fruitData.map {
          h =>
          val price = priceData.find(p => p.fruit == h.fruit && p.date == h.date).map(_.price).getOrElse(0.0)
          h.amount * price
        }.sum
        (fruit, earnings)
    }
    fruitEarnings.maxBy(_._2)._1
  }

  def bestEarningFruitPerMonth(monthlyIncome: Map[(Int, java.time.Month, String), List[Harvest]], priceData: List[Price]): Map[(Int, java.time.Month), String] = {
    monthlyIncome.groupBy {
      case ((year, month, fruit), _) => (year, month)
    }.map {
      case ((year, month), monthlyData) =>
        val bestEarningFruit = monthlyData.maxBy {
          case (_, harvests) =>
            harvests.map {
              h =>
                val price = priceData.find(p => p.fruit == h.fruit && p.date == h.date).map(_.price).getOrElse(0.0)
                h.amount * price
            }.sum
        }
        ((year, month), bestEarningFruit._1._3)
    }
  }

  def leastProfitableFruitOverall(harvestData: List[Harvest], priceData: List[Price]): String = {
    val fruitEarnings = harvestData.groupBy(_.fruit).map {
      case (fruit, data) =>
        val earnings = data.map {
          h =>
            val price = priceData.find(p => p.fruit == h.fruit && p.date == h.date).map(_.price).getOrElse(0.0)
            h.amount * price
        }.sum
      (fruit, earnings)
    }
    fruitEarnings.minBy(_._2)._1
  }

  def leastProfitableFruitPerMonth(monthlyIncome: Map[(Int, java.time.Month, String), List[Harvest]], priceData: List[Price]): Map[(Int, java.time.Month), String] = {
    monthlyIncome.groupBy {
        case ((year, month, fruit), _) => (year, month)
      }.map {
        case ((year, month), data) =>
        val leastFruit = data.minBy {
          case (_, harvests) =>
            harvests.map { h =>
              val price = priceData.find(p => p.fruit == h.fruit && p.date == h.date).map(_.price).getOrElse(0.0)
              h.amount * price
            }.sum
        }
        ((year, month), leastFruit._1._3)
      }
  }

  def topEarningGathererOverall(harvestData: List[Harvest], priceData: List[Price]): String = {
    val gathererEarnings = harvestData.groupBy(_.gatherer).map { case (gatherer, data) =>
      val earnings = data.map { h =>
        val price = priceData.find(p => p.fruit == h.fruit && p.date == h.date).map(_.price).getOrElse(0.0)
        h.amount * price
      }.sum
      (gatherer, earnings)
    }
    gathererEarnings.maxBy(_._2)._1
  }

  def topEarningGathererPerMonth(monthlyHarvest: Map[(Int, java.time.Month, String), List[Harvest]], priceData: List[Price]): Map[(Int, java.time.Month), String] = {
    monthlyHarvest.groupBy { case ((year, month, gatherer), _) => (year, month) }
      .map { case ((year, month), data) =>
        val topGatherer = data.maxBy { case (_, harvests) =>
          harvests.map { h =>
            val price = priceData.find(p => p.fruit == h.fruit && p.date == h.date).map(_.price).getOrElse(0.0)
            h.amount * price
          }.sum
        }
        ((year, month), topGatherer._1._3)
      }
  }
}
