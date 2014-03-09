package pl.mmaciaszek.geneticalgorithm.model
import scala.collection._

case class Genotype[T](val genes: mutable.Buffer[T])