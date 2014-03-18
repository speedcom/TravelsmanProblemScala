package pl.mmaciaszek.noevolutionary

import pl.mmaciaszek.geneticalgorithm.PopulationTSP
import scala.collection.mutable.MutableList

object RandomSearchTSP {

  private def getCosts(population: PopulationTSP) = population.parents.foldLeft(MutableList[Long]())((acc, el) => acc += el.cost)
 
  def getTheBestSolution(population: PopulationTSP) = getCosts(population).min
  def getTheWorstSolution(population: PopulationTSP) = getCosts(population).max
  def getAvgSolution(population: PopulationTSP) = getCosts(population).sum / population.parents.size
  
}