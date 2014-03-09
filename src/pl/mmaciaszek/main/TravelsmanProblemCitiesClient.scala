package pl.mmaciaszek.main

import pl.mmaciaszek.city.{LAHC_TSPlib, CitesTSP}
import pl.mmaciaszek.geneticalgorithm.PopulationTSP

object TravelsmanProblemCitiesClient {

  def main(args: Array[String]): Unit = {
    val cityFilePath =  "C:\\Users\\Mateusz\\workspace\\TravelsmanProblemScala\\resources\\a280.tsp";
    
    val cities = CitesTSP(LAHC_TSPlib.getMatrixDist(cityFilePath))
        
    println("cities.size = " + cities.getCitiesSize)
    val size = cities.getCitiesSize
//    var i = 0
//    var j = 0
//    println("test")
//    while(i <= size) {
//      while(j <= size) {
//        println("i = " + i + ", j= " + j + ", distance = " +  cities.getDistanceBetweenCities(i, j))
//        j += 1
//      }
//      i += 1
//      j = 0
    val probabilityOfCrossover = 0.7
    val probabilityOfMutation = 0.1
    val populationSize = 30
    val population = PopulationTSP(cities, populationSize)
    var iterationNumber = 20
    
    while(iterationNumber > 0) {
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
      
      iterationNumber -= 1
    }
    
    
  }

}