package pl.mmaciaszek.geneticalgorithm.model

object Phenotype {
  def apply[T,U](genotype: Genotype[T], evalFunc: Genotype[T] => U) = new Phenotype[T,U](genotype)(evalFunc)
}
class Phenotype[T, U](val genotype: Genotype[T])(val evalFunc: Genotype[T] => U) {
	var cost = evalFunc(genotype)
}