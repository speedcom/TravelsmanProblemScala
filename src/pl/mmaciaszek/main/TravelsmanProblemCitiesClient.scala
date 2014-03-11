package pl.mmaciaszek.main

import pl.mmaciaszek.city.{ LAHC_TSPlib, CitesTSP }
import pl.mmaciaszek.geneticalgorithm.PopulationTSP
import pl.mmaciaszek.geneticalgorithm.model.Phenotype
import scala.collection.mutable.MutableList

object TravelsmanProblemCitiesClient {

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  def findTheWorst(x: Phenotype[Long, Long], y: Phenotype[Long, Long]) = if (x.cost > y.cost) x else y
  def findTheBest(x: Phenotype[Long, Long], y: Phenotype[Long, Long]) = if (x.cost > y.cost) y else x

  def main(args: Array[String]): Unit = {

    val INSTANCES = 10
    var AVG = 0.0
    var STANDARD_DEVIATION = 0.0
    var THEBESTSOLUTIONS = MutableList[Long]()
    var THEWORSTSOLUTIONS = MutableList[Long]()

    val cityFilePath = "C:\\Users\\Mateusz\\workspace\\TravelsmanProblemScala\\resources\\a280.tsp"
    //    val cityFilePath = "C:\\Users\\Mateusz\\workspace\\TravelsmanProblemScala\\resources\\ch150.tsp"
    val cities = CitesTSP(LAHC_TSPlib.getMatrixDist(cityFilePath))
    println("Number of cities = " + cities.getCitiesSize)

    // options
    val probabilityOfCrossover = 0.7
    val probabilityOfMutation = 0.01
    val populationSize = 500
    val generationsNumber = 20

    for (i <- 1 to INSTANCES) {

      val population = PopulationTSP(cities, populationSize)
      population.evaluate(population.parents)

      val generationsData = new StringBuffer
      for (g <- 1 to generationsNumber) {

        population.children = population.selection(population.parents)
        population.children = population.crossover(population.children, probabilityOfCrossover)
        population.mutation(population.children, probabilityOfMutation)
        population.parents = population.children
        population.evaluate(population.parents)

        // do pliku z danej instancji zapisujemy:
        // - nr populacji
        // - theBest, theWorst, AVG osobnik
        val theBest = population.parents.reduceLeft(findTheBest).cost
        val theWorst = population.parents.reduceLeft(findTheWorst).cost
        val avg = population.parents.foldLeft(0L)((acc, b) => acc + b.cost) / population.parents.size
        val data = g + " " + theBest + " " + theWorst + " " + avg + "\n"
        generationsData.append(data)
      }
      import java.io._
      printToFile(new File("results\\r"+i+".txt"))(p => {
        p.println(generationsData.toString())
      })
      
      println("\n---------")
      println("Statistical data | INSTANCE nr " + i)

      val theBestSolution = population.parents.reduceLeft(findTheBest)
      THEBESTSOLUTIONS += theBestSolution.cost
      println("The best solution: " + theBestSolution.cost)

      val theWorstSolution = population.parents.reduceLeft(findTheWorst)
      THEWORSTSOLUTIONS += theWorstSolution.cost
      println("The worst solution: " + theWorstSolution.cost)

      val sumCost = population.parents.foldLeft(0L)((acc, b) => acc + b.cost)
      val avg = sumCost / population.parents.size
      AVG += avg
      println("AVG solution: " + avg)

      val w = population.parents.foldLeft(0.0)((acc, b) => acc + (b.cost - avg) * (b.cost - avg))
      val ww = w / population.parents.size
      val sigma = Math.sqrt(ww)
      STANDARD_DEVIATION += sigma
      println("Standard deviation: " + sigma)
      println("---------")
      
      
    }

    println("\nFINAL RESULTS")
    println("The best solution = " + THEBESTSOLUTIONS.min)
    println("The worst solution = " + THEWORSTSOLUTIONS.max)
    println("AVG solution = " + AVG / INSTANCES)
    println("Standard deviation = " + STANDARD_DEVIATION / INSTANCES)
  }
}