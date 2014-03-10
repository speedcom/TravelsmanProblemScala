package pl.mmaciaszek.main

import pl.mmaciaszek.city.{ LAHC_TSPlib, CitesTSP }
import pl.mmaciaszek.geneticalgorithm.PopulationTSP

object TravelsmanProblemCitiesClient {

  def main(args: Array[String]): Unit = {

    val cityFilePath = "C:\\Users\\Mateusz\\workspace\\TravelsmanProblemScala\\resources\\a280.tsp"
    val cities = CitesTSP(LAHC_TSPlib.getMatrixDist(cityFilePath))
    println("cities.size = " + cities.getCitiesSize)

    val probabilityOfCrossover = 0.7
    val probabilityOfMutation = 0.1
    val populationSize = 500
    val population = PopulationTSP(cities, populationSize)

    var iterationNumber = 100
    while (iterationNumber > 0) {
      println("---------")
      println("iteration number = " + iterationNumber)

      // evaluate
      println("starting evaluation...")
      population.evaluate(population.parents)
      println("end of evaluation")
      // selection
      println("starting selection...")
      population.children = population.selection(population.parents)
      println("end of selection.")
      // crossover
      println("starting crossover...")
      population.children = population.crossover(population.children, probabilityOfCrossover)
      println("end of crossover.")
      // mutation
      println("starting mutation...")
      population.mutation(population.children, probabilityOfMutation)
      println("end of mutation.")

      population.parents = population.children

      iterationNumber -= 1
    }
    println("\n---------")
    println("Dane statystyczne...")
    
  }

}