package pl.mmaciaszek.geneticalgorithm

import pl.mmaciaszek.city.CitesTSP
import scala.util.Random
import scala.collection.mutable.MutableList
import pl.mmaciaszek.geneticalgorithm.model.{ Genotype, Phenotype, Population }

object PopulationTSP {
  def apply(cities: CitesTSP, populationSize: Int) = {

    def evalFunc(genotype: Genotype[Long]): Long = {
      val genes = genotype.genes
      var sum = 0L
      for (t <- 1 until genes.size) {
        sum += cities.getDistanceBetweenCities(genes(t-1).toInt, genes(t).toInt)
      }
      sum += cities.getDistanceBetweenCities(genes(0).toInt, genes(genes.size - 1).toInt)
      sum
    }

    def createPhenotypes = {

      val indexes = (1L to cities.getCitiesSize).toList
      val phenotypes = MutableList[Phenotype[Long, Long]]()

      def createPhenotype = {
        var genes = MutableList[Long]()
        do {
          val g = Random.shuffle(indexes)
          genes = g.foldLeft(MutableList[Long]())((acc, e) => acc += e)
        } while(phenotypes.exists(_.genotype.genes equals genes))
        val genotype = Genotype(genes)
        val phenotype = Phenotype(genotype, 0L)(evalFunc)
        phenotype
      }

      def addPhenotype(phenotype: Phenotype[Long, Long]) = phenotypes += phenotype

      for(t <- 1 to populationSize) {
        addPhenotype(createPhenotype)
      }
      phenotypes
    }

    new PopulationTSP {
      parents = createPhenotypes
    }
  }

  def chooseTheBestPhenotype(x: Phenotype[Long, Long], y: Phenotype[Long, Long]) = if (x.cost > y.cost) y else x
  def chooseTheWorstPhenotype(x: Phenotype[Long, Long], y: Phenotype[Long, Long]) = if (x.cost > y.cost) x else y
}
class PopulationTSP extends Population[Long, Long] {
  override type PhenotypeType = Phenotype[Long, Long]

  def evaluate(x: MutableList[PhenotypeType]): Unit = x foreach { y => if(y.cost == 0) { y.evaluateCost} }
  
  // Tournament Selection
  def selection(phenotypes: MutableList[PhenotypeType]): MutableList[PhenotypeType] = {
    
    val phenotypesSize = phenotypes.size
    val tournamentSize = if (phenotypesSize < 4) phenotypesSize else 4
    val children = MutableList[PhenotypeType]()
    
    (1 to phenotypesSize) foreach { t =>
      // choose participants to tournament and select the best one from them
      val randomlyChoosenIndexes = Seq.fill(tournamentSize)(Random.nextInt(phenotypesSize))
      val tournamentParticipants = randomlyChoosenIndexes.foldLeft(MutableList[PhenotypeType]())((acc, i) => acc += phenotypes(i))
      val winner = tournamentParticipants.reduceLeft(PopulationTSP.chooseTheBestPhenotype)
      children += winner
    }
    children
  }

  // Order Crossover
  def crossover(phenotypes: MutableList[PhenotypeType], probability: Double): MutableList[PhenotypeType] = {

    def createSubIndex(phenotype: PhenotypeType) = {
      val genotypeSize = phenotype.genotype.genes.size
      val i = Random.nextInt(genotypeSize - 1)
      val rangeTmp = i + 1 to genotypeSize - 1
      val j = rangeTmp(Random.nextInt(rangeTmp length))

      (i, j)
    }
    def createChild(parent1: PhenotypeType, parent2: PhenotypeType)(subIndex: Tuple2[Int, Int]) = {
      val genotypeSize = parent1.genotype.genes.size
      val genesX = parent1.genotype.genes.slice(subIndex._1, subIndex._2)
      // inversion of second fenotype genes
      val genesY = parent2.genotype.genes.slice(subIndex._2, genotypeSize) ++ parent2.genotype.genes.slice(0, subIndex._2)
      
      var t = genotypeSize - subIndex._2
      val genesXright = genesX
      genesY foreach { x =>
        if (t > 0 && !genesX.exists(_ == x)) {
          genesXright += x
          t -= 1
        }
      }
      
      t = subIndex._1
      val genesXleft = scala.collection.mutable.MutableList[Long]()
      genesY foreach { x =>
        if (t > 0 && !genesX.exists(_ == x)) {
          genesXleft += x
          t -= 1
        }
      }
      val finalGenes = genesXleft ++ genesXright
      val genotype = Genotype(finalGenes)
      val fenotype = Phenotype(genotype, cost = 0L)(parent1.evalFunc)

      fenotype
    }
    def createChilds(parent1: PhenotypeType, parent2: PhenotypeType)(subIndex: Tuple2[Int, Int]) = {
      // create childs
      val subIndex = createSubIndex(parent1)
      val fenotype1 = createChild(parent1, parent2)(subIndex)
      val fenotype2 = createChild(parent2, parent1)(subIndex)

      (fenotype1, fenotype2)
    }

    val crossOveredPhenotypes = MutableList[PhenotypeType]()
    val phenotypeSize = phenotypes.size/2 // it's beacause parents create two childs
    (1 to phenotypeSize) foreach { t =>
      val i = Random.nextInt(phenotypes.size - 1)
      val j = Random.nextInt(phenotypes.size - 1)

      val p1 = phenotypes(i)
      val p2 = phenotypes(j)

      val r = Random.nextDouble
      if (r <= probability) {
        val childs = createChilds(p1, p2)(createSubIndex(p1))
        crossOveredPhenotypes += childs._1
        crossOveredPhenotypes += childs._2
      } else {
        crossOveredPhenotypes += p1
        crossOveredPhenotypes += p2
      }
    }
    crossOveredPhenotypes
  }

  // Inversion Mutation
  def mutation(phenotypes: MutableList[PhenotypeType], probability: Double): Unit = {
    
    phenotypes foreach { x => 
      val r = Random.nextDouble
      if (r <= probability) {
        val ceiling = x.genotype.genes.size
        val j = Random.nextInt(ceiling)
        val k = Random.nextInt(ceiling)

       // swap values of choosen genes
       val l = x.genotype.genes(j)
       x.genotype.genes(j) = x.genotype.genes(k)
       x.genotype.genes(k) = l
      }
    }
  }
}
