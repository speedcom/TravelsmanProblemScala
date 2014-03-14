package pl.mmaciaszek.main

import pl.mmaciaszek.city.{ LAHC_TSPlib, CitesTSP }
import pl.mmaciaszek.geneticalgorithm.PopulationTSP
import pl.mmaciaszek.geneticalgorithm.model.Phenotype
import scala.collection.mutable.MutableList
import pl.mmaciaszek.noevolutionary.RandomSearchTSP

object TravelsmanProblemCitiesClient {

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  def main(args: Array[String]): Unit = {

    // results
    var AVG = 0.0
    var STANDARD_DEVIATION = 0.0
    var THEBESTSOLUTIONS = MutableList[Long]()
    var THEWORSTSOLUTIONS = MutableList[Long]()

    // options   
    val instancesCount = 2
    val probabilityOfCrossover = 0.7
    val probabilityOfMutation = 0.01
    val populationSize = 50
    val generationsNumber = 10

    // city
    val cityFilePath = System.getProperty("user.dir") + "\\resources\\a280.tsp"
    val cities = CitesTSP(LAHC_TSPlib.getMatrixDist(cityFilePath))
    println("Number of cities = " + cities.getCitiesSize)

    (1 to instancesCount) foreach { i =>

      val population = PopulationTSP(cities, populationSize)
      population.evaluate(population.parents)

      val generationsData = new StringBuffer
      (1 to generationsNumber) foreach { g =>

        population.children = population.selection(population.parents)
        population.children = population.crossover(population.children, probabilityOfCrossover)
        population.mutation(population.children, probabilityOfMutation)
        population.parents = population.children
        population.evaluate(population.parents)

        val theBest = population.parents.reduceLeft(PopulationTSP.chooseTheBestPhenotype).cost
        val theWorst = population.parents.reduceLeft(PopulationTSP.chooseTheWorstPhenotype).cost
        val avg = population.parents.foldLeft(0L)((acc, b) => acc + b.cost) / population.parents.size
        val data = g + " " + theBest + " " + theWorst + " " + avg + "\n"
        generationsData.append(data)
      }

      import java.io._
      printToFile(new File("results\\instance_nr_" + i + ".txt"))(p => {
        val labels = "Population TheBest TheWorst AVG\n"
        p.println(labels + generationsData.toString())
      })

      println("\n---------")
      println("Statistical data | INSTANCE nr " + i)

      val theBestSolution = population.parents.reduceLeft(PopulationTSP.chooseTheBestPhenotype)
      THEBESTSOLUTIONS += theBestSolution.cost
      println("The best solution: " + theBestSolution.cost)

      val theWorstSolution = population.parents.reduceLeft(PopulationTSP.chooseTheWorstPhenotype)
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

    println("\nSTATISTICAL RESULTS FROM ALL INSTANCES")
    println("The best solution = " + THEBESTSOLUTIONS.min)
    println("The worst solution = " + THEWORSTSOLUTIONS.max)
    println("AVG solution = " + AVG / instancesCount)
    println("Standard deviation = " + STANDARD_DEVIATION / instancesCount)

    println("\nSTATISTICAL RESULT FOR NOEVOLUTIONARY ALGHORITM")
    val population = PopulationTSP(cities, populationSize)
    population.evaluate(population.parents)
    println("The best solution = " + RandomSearchTSP.getTheBestSolution(population))
    println("The worst solution = " + RandomSearchTSP.getTheWorstSolution(population))
    println("AVG solution = " + RandomSearchTSP.getAvgSolution(population))

  }
}