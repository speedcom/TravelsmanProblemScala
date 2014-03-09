package pl.mmaciaszek.city

case class CitesTSP(val matrixDistances: Array[Array[Long]]) {	
    def getCitiesSize = matrixDistances.size
	def getDistanceBetweenCities(i: Int, j: Int) = matrixDistances(i)(j)
}