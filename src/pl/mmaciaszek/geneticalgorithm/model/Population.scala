package pl.mmaciaszek.geneticalgorithm.model

import scala.collection.mutable.MutableList
import pl.mmaciaszek.geneticalgorithm.alghoritm.PopulationAlghoritm

abstract class Population[T, U] extends PopulationAlghoritm[T, U] {
  type PhenotypeType = Phenotype[T, U]
  var parents = MutableList[PhenotypeType]()
  var children = MutableList[PhenotypeType]()
}