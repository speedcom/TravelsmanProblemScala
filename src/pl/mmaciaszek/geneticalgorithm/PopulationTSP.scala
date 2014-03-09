package pl.mmaciaszek.geneticalgorithm

import pl.mmaciaszek.city.CitesTSP
import scala.util.Random
import scala.collection.mutable.MutableList
import pl.mmaciaszek.geneticalgorithm.model.{ Genotype, Phenotype, Population }

object PopulationTSP {
  def apply(cities: CitesTSP, populationSize: Int) = {
    def createPhenotypes = {
      val indexes = (1L to cities.getCitiesSize).toList
      val phenotypes = MutableList[Phenotype[Long, Long]]()
      def createPhenotype = {
        val genes = Random.shuffle(indexes)
        val genotype = Genotype(genes.toBuffer)
        val phenotype = Phenotype(genotype, (x: Genotype[Long]) => x.genes.sum)
        phenotype
      }
      def addPhenotype(phenotype: Phenotype[Long, Long]) = phenotypes += phenotype

      (1 to populationSize) foreach {
        addPhenotype(createPhenotype)
      }
      phenotypes
    }

    new PopulationTSP {
      parents = createPhenotypes
    }
  }
}
class PopulationTSP extends Population[Long, Long] {
  override type PhenotypeType = Phenotype[Long, Long]

  def evaluate(x: MutableList[PhenotypeType]): Unit = x foreach { _.evalFunc }

  // Tournament Selection
  def selection(phenotypes: MutableList[PhenotypeType]): MutableList[PhenotypeType] = {
    val phenotypesSize = phenotypes.size
    val tournamentSize = if (phenotypesSize < 4) phenotypesSize else 4

    val children = MutableList[PhenotypeType]()
    (1 to phenotypesSize) foreach {

      // choose participants to tournament
      val randomlyChoosenIndexes = Seq.fill(tournamentSize)(Random.nextInt(phenotypesSize))
      val tournamentParticipants = randomlyChoosenIndexes.foldLeft(MutableList[PhenotypeType]())((acc, i) => acc += phenotypes(i))

      // select the best participant 
      def haveFight(x: PhenotypeType, y: PhenotypeType) = if (x.cost > y.cost) x else y
      val winner = tournamentParticipants.reduceLeft(haveFight)

      children += winner
    }
    children
  }

  // Order Crossover
  def crossover(phenotypeX: PhenotypeType, phenotypeY: PhenotypeType): (PhenotypeType, PhenotypeType) = {

    def createSubIndex = {
      val genotypeSize = phenotypeX.genotype.genes.size
      val i = Random.nextInt(genotypeSize - 1)
      val rangeTmp = i + 1 to genotypeSize - 1
      val j = rangeTmp(Random.nextInt(rangeTmp length))
      
      (i, j)      
    }

    def createChild(parent1: PhenotypeType, parent2: PhenotypeType)(subIndex: Tuple2[Int, Int]) = {
      val genotypeSize = parent1.genotype.genes.size
      val genesX = parent1.genotype.genes.slice(subIndex._1, subIndex._2)
      // inversion of second fenotype genes
      val genesY = parent2.genotype.genes.slice(subIndex._2 + 1, genotypeSize) ++ phenotypeY.genotype.genes.slice(0, subIndex._2)

      var t = genotypeSize - subIndex._2 + 1;
      val genesXright = genesX
      genesY foreach { x =>
        if (t > 0 && !genesX.exists(_ == x)) {
          genesXright += x
          t -= 1
        }
      }

      t = subIndex._1 - 1
      val genesXleft = scala.collection.mutable.Buffer[Long]()
      genesY foreach { x =>
        if (t > 0 && !genesX.exists(_ == x)) {
          genesXleft += x
          t -= 1
        }
      }
      val finalGenesX = genesXleft ++ genesXright

      val genotype = Genotype(finalGenesX)
      val fenotype = Phenotype(genotype, phenotypeX.evalFunc)

      fenotype
    }

    // create childs
    val subIndex = createSubIndex
    val fenotype1 = createChild(phenotypeX, phenotypeY)(subIndex)
    val fenotype2 = createChild(phenotypeY, phenotypeX)(subIndex)

    (fenotype1, fenotype2)
  }

  // Inversion Mutation
  def mutation(x: PhenotypeType, probability: Double): PhenotypeType = {
    val r = Random.nextDouble
    if (r <= probability) {
      val ceiling = x.genotype.genes.size
      val j = Random.nextInt(ceiling)
      val k = Random.nextInt(ceiling)

      // swap values of choosen genes
      val l = x.genotype.genes(j)
      x.genotype.genes(j) = x.genotype.genes(k)
      x.genotype.genes(k) = l

      x
    } else x
  }
}
