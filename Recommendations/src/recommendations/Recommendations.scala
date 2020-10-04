package recommendations

import scala.math._

object Recommendations {

  def standardDeviation[T](elements: List[T], property: T => Double): Double = {
    val list:List[Double] = for{element <- elements}yield property(element)
    val mean:Double = list.sum/list.length
    val summation:Double = (for {number <- list}yield Math.pow(number-mean, 2)).sum
    val test = Math.sqrt(summation/(elements.length - 1))
    return test
  }

  // population or sample? sample coded here
  def correlation[T](elements: List[T], property1: T => Double, property2: T => Double): Double = {

    val listOne:List[Double] = for {element <- elements}yield property1(element)
    val listTwo:List[Double] = for {element <- elements}yield property2(element)
    val map = for {element <- elements} yield (property1(element) -> property2(element))
    val stdDev1 = standardDeviation(elements, property1)
    val stdDev2 = standardDeviation(elements, property2)
    val mean1 = listOne.sum/listOne.length
    val mean2 = listTwo.sum/listTwo.length
    val top:Double = (for {element <- map} yield ((element._1-mean1)/stdDev1)*((element._2-mean2)/stdDev2)).sum
    top/(elements.length-1)
  }


  def topKCorrelations[T](elements: List[T], allProperties: Map[String, T => Double], testPropertyName: String, k: Int): List[String] = {
    val names:List[String] = (for {name <- allProperties.keys} yield name).toList
    val correlations:List[Double] = (for {name <- names} yield correlation(elements, allProperties(testPropertyName), allProperties(name)))
    val mapNameCor:Map[String, Double] = (for {i <- 0 until names.length} yield (names(i) -> correlations(i))).toMap
    val sortedCor = correlations.sortWith((a:Double, b:Double) => a > b)
    println(mapNameCor)
    println(sortedCor)
    val ay:List[String] = for {cor <- sortedCor; (key, value) <- mapNameCor
                                   if cor == value} yield key
    val retVal:List[String] = (for {i <- 1 to k} yield ay(i)).toList
    retVal
  }

}
