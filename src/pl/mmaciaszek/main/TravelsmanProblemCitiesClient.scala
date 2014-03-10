package pl.mmaciaszek.main

import pl.mmaciaszek.city.{ LAHC_TSPlib, CitesTSP }
import pl.mmaciaszek.geneticalgorithm.PopulationTSP
import pl.mmaciaszek.geneticalgorithm.model.Phenotype
import scala.collection.mutable.MutableList

object TravelsmanProblemCitiesClient {

  def main(args: Array[String]): Unit = {

    val INSTANCES = 10
    var AVG = 0.0
    var STANDARD_DEVIATION = 0.0
    var THEBESTSOLUTION = MutableList[Long]()
    var THEWORSTSOLUTION = MutableList[Long]()

    val cityFilePath = "C:\\Users\\Mateusz\\workspace\\TravelsmanProblemScala\\resources\\a280.tsp"
    //    val cityFilePath = "C:\\Users\\Mateusz\\workspace\\TravelsmanProblemScala\\resources\\ch150.tsp"
    val cities = CitesTSP(LAHC_TSPlib.getMatrixDist(cityFilePath))
    println("Number of cities = " + cities.getCitiesSize)

    // options
    val probabilityOfCrossover = 0.7
    val probabilityOfMutation = 0.01
    val populationSize = 500
    val generationsNumber = 100

    for (i <- 1 to INSTANCES) {

      val population = PopulationTSP(cities, populationSize)
      population.evaluate(population.parents)

      for (i <- 1 to generationsNumber) {

        // selection
        //println("starting selection...")
        population.children = population.selection(population.parents)
        //println("end of selection.")
        // crossover
        //println("starting crossover...")
        population.children = population.crossover(population.children, probabilityOfCrossover)
        //println("end of crossover.")
        // mutation
        //println("starting mutation...")
        population.mutation(population.children, probabilityOfMutation)
        //println("end of mutation.")

        population.parents = population.children

        // evaluate
        //println("starting evaluation...")
        population.evaluate(population.parents)
        //println("end of evaluation")
      }
      println("\n---------")
      println("Dane statystyczne | Instacja nr " + i)

      // najlepszy osobnik z iteracji
      type PhenotypeType = Phenotype[Long, Long]
      def findTheBest(x: PhenotypeType, y: PhenotypeType) = if (x.cost > y.cost) y else x
      val theBestSolution = population.parents.reduceLeft(findTheBest)
      THEBESTSOLUTION += theBestSolution.cost
      println("Najlepszy osobnik: " + theBestSolution.cost)

      // najgorszy osobnik z iteracji
      def findTheWorst(x: PhenotypeType, y: PhenotypeType) = if (x.cost > y.cost) x else y
      val theWorstSolution = population.parents.reduceLeft(findTheWorst)
      THEWORSTSOLUTION += theWorstSolution.cost
      println("Najgorszy osobnik: " + theWorstSolution.cost)

      // srednia wartosc w populacji dla iteracji
      val sumCost = population.parents.foldLeft(0L)((acc, b) => acc + b.cost)
      val avg = sumCost / population.parents.size
      AVG += avg
      println("Srednia wartosc w populacji: " + avg)

      // odchylenie standardowe dla iteracji
      val w = population.parents.foldLeft(0.0)((acc, b) => acc + (b.cost - avg) * (b.cost - avg))
      val ww = w / population.parents.size
      val sigma = Math.sqrt(ww)
      STANDARD_DEVIATION += sigma
      println("Odchylenie standardowe: " + sigma)
      println("---------")
    }
    
    println("Najlepszy osobnik = " + THEBESTSOLUTION.min)
    println("Najgorszy osobnik = " + THEWORSTSOLUTION.max)
    println("Œredni osobnik AVG = " + AVG/INSTANCES)
    println("Odchylenie standardowe AVG = " + STANDARD_DEVIATION/INSTANCES)
  }

}