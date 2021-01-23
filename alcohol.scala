// Part 2 about Alcohol-Consumption Worldwide
//============================================

object CW6b {

import io.Source
import scala.util._

val url_alcohol = 
  "https://raw.githubusercontent.com/fivethirtyeight/data/master/alcohol-consumption/drinks.csv"

val file_population = 
  "population.csv"

  
//(1) get_csv_page function below. It takes a URL-string
//    as argument and generates a list of strings corresponding to each
//    line in the downloaded csv-list. 

def get_csv_page(url: String) : List[String] = {
	
	val pageAsString = Try(Source.fromURL(url)("ISO-8859-1").take(10000).mkString).getOrElse { println(s"  Problem with: $url"); ""}
	val urlLinesList = pageAsString.split("\n").toList

	process_alcs(urlLinesList)
	
	urlLinesList
}

//    get_csv_file function below. It takes a file name 
//    as argument and reads the content of the given file. Like above,
//    it should generate a list of strings corresponding to each
//    line in the csv-list. The filename file_population is one possible
//    argument.

def get_csv_file(file: String) : List[String] = {

	val fileAsString = Try(Source.fromFile(file).mkString).getOrElse { println(s"  Problem with: $file"); ""}
	val fileLinesList = fileAsString.split("\n").toList
	
	process_pops(fileLinesList)
	
	fileLinesList
	
}

//(2) functions that process the csv-lists. For
//    process_alcs extract the country name (as String) and the 
//    pure alcohol consumption (as Double). For process_pops
//    generate a Map of Strings (country names) to Long numbers 
//    (population sizes). 

def process_alcs(lines: List[String]) : List[(String, Double)] = {

	if (Try(lines(0).split(",")(4).toDouble).isFailure)	{	// Does the first line contain numbers or not? remove first line if it does not.
		val newList = lines.drop(1)
		
		val listOfNewPairs = for (line <- newList)
		yield (line.split(",")(0), line.split(",")(4).toDouble)
	
		listOfNewPairs
	}
	
	else { 
		val newList = lines 
		
		val listOfNewPairs = for (line <- newList)
		yield (line.split(",")(0), line.split(",")(4).toDouble)
	
		listOfNewPairs

	}
	
}

def process_pops(lines: List[String]) : Map[String, Long] = {

	if (Try(lines(0).split(",")(1).toLong).isFailure)	{	// Does the first line contain numbers or not? remove first line if it does not.
		val newList = lines.drop(1) 
		
		val listOfNewPairs = for (line <- newList)
		yield (line.split(",")(0), line.split(",")(1).toLong)
		
		val mapOfPairs = listOfNewPairs.toMap
		mapOfPairs
	}
	
	else { 
		val newList = lines 
		
		val listOfNewPairs = for (line <- newList)
		yield (line.split(",")(0), line.split(",")(1).toLong)
		
		val mapOfPairs = listOfNewPairs.toMap
		mapOfPairs
		
	}

}

//(3) Calculate for each country the overall alcohol_consumption using
//    the data from the alcohol list and the population sizes list. 
//	  only include countries on the alcohol list that are also
//    on the population sizes list with the exact same name. Note that
//    the spelling of some names in the alcohol list differs from the
//    population sizes list. You can ignore entries where the names differ. 
//    Sort the resulting list according to the country with the highest alcohol 
//    consumption to the country with the lowest alcohol consumption.

def sorted_country_consumption() : List[(String, Long)] = {
	
	val url = """https://raw.githubusercontent.com/fivethirtyeight/data/master/alcohol-consumption/drinks.csv""" 
	val file = "population.csv"

	val listCountryToAlc = process_alcs(get_csv_page(url))
	val mapCountryToPop = process_pops(get_csv_file(file))
	
	val listCountryConsum = 
	for (n <- listCountryToAlc;
	if(mapCountryToPop.isDefinedAt(n._1)))
	yield (n._1, (mapCountryToPop.get(n._1).get * n._2).toLong) // Lifting other as option(Long) and tryget may be 'safer'
	val listCountryConsumSorted = listCountryConsum.sortBy(_._2).reverse

	listCountryConsumSorted

}

//   Calculate the world consumption of pure alcohol of all countries, which 
//   should be the first element in the tuple below. The second element is
//   the overall consumption of the first n countries in the sorted list
//   from above; and finally the double should be the percentage of the 
//   first n countries drinking from the the world consumption of alcohol.          

def percentage(n: Int) : (Long, Long, Double) = {

	val listCountryConsumSorted = sorted_country_consumption()
	
	// For total alcohol consumption:
	val newList =
	for (n <- listCountryConsumSorted)
	yield n._2
	val totalAlcConsum = newList.sum
	
	// For n-highest alcohol consumers:
	val nHighest = listCountryConsumSorted.take(n)	
	val nHighestList =
	for (n <- nHighest)
	yield n._2
	val nHighestSum = nHighestList.sum

	// For percentage:
	val percentage = ((nHighestSum.toFloat / totalAlcConsum) * 100).toDouble // make the calculation a float
		
	(totalAlcConsum, nHighestSum, percentage)

}

}