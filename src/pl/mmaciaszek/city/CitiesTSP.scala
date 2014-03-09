package pl.mmaciaszek.city

case class CitesTSP(val matrixDistances: Array[Array[Long]]) {	
    def getCitiesSize = matrixDistances.size-1
	def getDistanceBetweenCities(i: Int, j: Int) = { 
	  matrixDistances(i)(j)
    }
}