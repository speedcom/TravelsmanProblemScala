package pl.mmaciaszek.geneticalgorithm.model

object Phenotype {
  def apply[T,U](genotype: Genotype[T], cost: U)(evalFunc: Genotype[T] => U) = new Phenotype[T,U](genotype, cost)(evalFunc)
}
class Phenotype[T, U](val genotype: Genotype[T], var cost: U)(val evalFunc: Genotype[T] => U) {	
  def evaluateCost = { cost = evalFunc(genotype) }  
}