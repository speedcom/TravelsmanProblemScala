package pl.mmaciaszek.main

import pl.mmaciaszek.city.{ LAHC_TSPlib, CitesTSP }
import pl.mmaciaszek.geneticalgorithm.PopulationTSP
import pl.mmaciaszek.geneticalgorithm.model.Phenotype

object TravelsmanProblemCitiesClient {

  def main(args: Array[String]): Unit = {

//    val cityFilePath = "C:\\Users\\Mateusz\\workspace\\TravelsmanProblemScala\\resources\\a280.tsp"
//    val cityFilePath = "C:\\Users\\Mateusz\\workspace\\TravelsmanProblemScala\\resources\\ali535.tsp"
//val cityFilePath = "C:\\Users\\Mateusz\\workspace\\TravelsmanProblemScala\\resources\\att532.tsp"
    val cityFilePath = "C:\\Users\\Mateusz\\workspace\\TravelsmanProblemScala\\resources\\ch150.tsp"
    val cities = CitesTSP(LAHC_TSPlib.getMatrixDist(cityFilePath))
    println("cities.size = " + cities.getCitiesSize)

    // options
    val probabilityOfCrossover = 0.7
    val probabilityOfMutation = 0.1
    val populationSize = 500
    val generationsNumber = 100

    val population = PopulationTSP(cities, populationSize)
    population.evaluate(population.parents)
    
    for (i <- 1 to generationsNumber) {
      println("---------")
      println("iteration number = " + i)

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
      
      // evaluate
      println("starting evaluation...")
      population.evaluate(population.parents)
      println("end of evaluation")
    }
    println("\n---------")
    println("Dane statystyczne...")
    
    // najlepszy osobnik
    type PhenotypeType = Phenotype[Long, Long]
    def findTheBest(x: PhenotypeType, y: PhenotypeType) = if(x.cost > y.cost) y else x
    val theBestSolution = population.parents.reduceLeft(findTheBest)
    println("Najlepszy osobnik = " + theBestSolution.cost)
    
    // najgorszy osobnik
    def findTheWorst(x: PhenotypeType, y: PhenotypeType) = if(x.cost > y.cost) x else y
    val theWorstSolution = population.parents.reduceLeft(findTheWorst)
    println("Najgorszy osobnik = " + theWorstSolution.cost)
    
    // srednia wartosc w populacji
    val sumCost = population.parents.foldLeft(0L)((acc, b) => acc + b.cost)
    val avg = sumCost / population.parents.size
    println("Srednia wartosc w populacji = " + avg)
    
    // odchylenie standardowe
    val w = population.parents.foldLeft(0.0)((acc, b) => acc + (b.cost-avg)*(b.cost-avg))
    val ww = w / population.parents.size
    val sigma = Math.sqrt(ww)
    println("Odchylenie standardowe = " + sigma)
  }

}