package pl.mmaciaszek.main

import pl.mmaciaszek.city.{LAHC_TSPlib, CitesTSP}

object TravelsmanProblemCitiesClient {

  def main(args: Array[String]): Unit = {
    val cityFilePath =  "C:\\Users\\Mateusz\\workspace\\TravelsmanProblemScala\\resources\\a280.tsp";
    
    val cities = CitesTSP(LAHC_TSPlib.getMatrixDist(cityFilePath))
        
  }

}