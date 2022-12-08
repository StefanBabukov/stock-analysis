import scala.io.Source
import scala.io.StdIn.readLine

object Main extends App{
  
  def readFileAddToMap(filename: String): Map[String,List[Int]] = {
    // create buffer to build up map as we read each line
    var mapBuffer: Map[String, List[Int]] = Map()
    try {
      for (line <- Source.fromFile(filename).getLines()) {     
        val splitline = line.split(",").map(_.trim).toList     
        mapBuffer = mapBuffer ++ Map(splitline.head -> splitline.tail.map(s => s.toInt))
      }
    } catch {
      case ex: Exception => println( ex)
    }
    mapBuffer
  }

  def getStockList(stockName: String, stockData: Map[String, List[Int]]):List[Int] = {
    var stockList = stockData.get(stockName)
    stockList match {
        case Some(value) => value
        case None => throw new Exception("Stock not found, please try again!")
    }
  }

  /////////////////// analyse functions \\\\\\\\\\\\\\\\\\\\\\\\\

  def getCurrentStockPrice(stock: String, stockList: List[Int]): Int  = {
    val currentPrice = stockList.last
    println(s"$stock: $currentPrice")
    currentPrice
  }

  def getHighestAndLowest(stock: String, stockList: List[Int]): Any = {
    var highest = stockList.max
    var lowest = stockList.min

    println(s"For stock $stock \n" +
            s"Highest price: $highest \n" +
            s"Lowest price: $lowest")
  }

  def stockWithHighestRise(stockMap: Map[String, List[Int]]): Any = {
    var maxSevenDayRise = 0;
    var maxSevenDayRiseStock = ""
    
    stockMap.keys.foreach(stock=>{
      val currentListReversed = getStockList(stock, stockMap).reverse
      val lastDay = currentListReversed(0)
      val oneWeekAgo = currentListReversed(6)
      val currentStockRise = lastDay - oneWeekAgo

      if(currentStockRise > maxSevenDayRise){
        maxSevenDayRise = currentStockRise
        maxSevenDayRiseStock = stock;
      }
    })

    println(s"Highest rise stock is $maxSevenDayRiseStock - $maxSevenDayRise")
  }

  def getMedianValue(stock: String, stockList: List[Int]): Any = {
    var sortedList = stockList.sortWith(_ < _)
    var medianValue = 0.0;

    if (sortedList.size % 2 == 1) medianValue = sortedList.size / 2
    else {
      val (up, down) = sortedList.splitAt(sortedList.size / 2)
      println(s"$up $down")

      medianValue = (up.last + down.head).toFloat/2
    }

    println(s"Median value for stock $stock is $medianValue")
  }

  def getAverageValue(stockList: List[Int]): Float = {
    stockList.sum / stockList.length
  }

  def compareTwoStocks(stockNames: List[String], stockMap: Map[String, List[Int]]): Any = {
    val firstStock = stockNames(0)
    val secondStock= stockNames(1)

    val firstAverage = getAverageValue(getStockList(firstStock, stockMap))
    val secondAverage = getAverageValue(getStockList(secondStock, stockMap))

    var outputString = ""
    var comparisonString = "";
    val stockComparison = firstAverage - secondAverage;

    if(stockComparison > 0){
      comparisonString = "bigger than"
    } else if (stockComparison==0){
      comparisonString = "the same as"
    } else {
      comparisonString = "less than"
    }

    outputString = s"$firstStock is $comparisonString $secondStock with averages: \n" +
      s"$firstStock: $firstAverage \n" +
      s"$secondStock: $secondAverage"
    println(outputString)
    outputString
  }

  def getPortfolioValue(ownedStocks: scala.collection.mutable.Map[String, Int], stockMap: Map[String, List[Int]]) : Any ={
    // var portfolioValue = Map[String, Int]()
    var outputString = "Current portfolio: "
    for (stock <- ownedStocks.keys) {
      val currentStockList = getStockList(stock, stockMap)
      val valueOwned = getCurrentStockPrice(stock, currentStockList) * ownedStocks.getOrElse(stock, 0)

      outputString += s"\n $stock: $valueOwned"
    }
    println(outputString)
    outputString
  }

  def forEveryStock(stockMap: Map[String, List[Int]], stockFunc: (String, List[Int]) => Any) : Any = {
    for (stock <- stockMap.keys) {
      val currentStockList = getStockList(stock, stockMap)
      stockFunc(stock, currentStockList)
    }
  }

  val mapdata = readFileAddToMap("../../../data.txt")
  
  println("Stock information. Please select an analysis you would like to perform. By typing the corresponding number and pressing Enter. \n" +
    "1. Current price for each stock \n" +
    "2. Highest and Lowest prices for each stock \n" +
    "3. Median price of each stock \n" +
    "4. Stock which has risen the most in the last 7 days \n" +
    "5. Compare the average price of two stocks \n" +
    "6. Input portfolio and get current stock values \n" +
    "7. Exit \n \n" +
    "Type the number and hit 'Enter': \n"
    )
  val analysisOption = 0

  while(analysisOption != 7 ){
  val analysisOption = readLine()

  try{
    analysisOption.toInt match {
        case 1 => {
          println("Current price for each stock: /n")
          forEveryStock(mapdata, getCurrentStockPrice)
        }
        case 2 => {
          println("Highest and lowest prices for each stock: \n")
          forEveryStock(mapdata, getHighestAndLowest)
        }
        case 3 => {
          println("Median price for each stock: \n")
          forEveryStock(mapdata, getMedianValue)
        }
        case 4 => {
          stockWithHighestRise(mapdata)      
        }
        case 5 => {
          println("Input the two stocks you want to compare, separated by a comma and press Enter \n" +
            "Example: AAPL, MET \n")
          val input = readLine()
          val stocksToCompare = input.split(",").map(_.trim).toList
          compareTwoStocks(stocksToCompare, mapdata)
        }
        case 6 =>{
          println("Input stock and number of shares owned, separated by a comma and press Enter \n" +
            "Example: AAPL, 1, SK1, 3, SK3, 5")
          val input = readLine()
          val ownedStocks = input.split(",").map(_.trim)
          val portfolioMap = scala.collection.mutable.Map[String, Int]()
          val maxIterations = ownedStocks.length
          for (i <- 0 until maxIterations by 2) {
            val stock = ownedStocks(i).toString
            val numOfShares = ownedStocks(i+1).toInt
            portfolioMap.put(stock, numOfShares)
          }
          getPortfolioValue(portfolioMap, mapdata)
        }
        case 7=>System.exit(0)
        case default => println("Unknown command, please try again! \n")
    }
  } catch {
    case e : Exception => println("Something went wrong, make sure you put the correct input and try again")
  }
  }
}
