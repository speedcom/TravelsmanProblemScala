package pl.mmaciaszek.geneticalgorithm.alghoritm

import pl.mmaciaszek.geneticalgorithm.model.Phenotype
import scala.collection.mutable.MutableList

trait PopulationAlghoritm[T, U] {
	def evaluate(x: MutableList[Phenotype[T, U]]): Unit
    def selection(x: MutableList[Phenotype[T, U]]): MutableList[Phenotype[T, U]]
    def crossover(x: MutableList[Phenotype[T, U]], probability: Double): MutableList[Phenotype[T, U]]
	def mutation(x: Phenotype[T, U], probability: Double): Phenotype[T,U]
}