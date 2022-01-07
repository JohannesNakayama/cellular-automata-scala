import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object CellularAutomaton {

  def ruleAsBinary(rule: Int): String = rule.toBinaryString.reverse.padTo(8, '0').reverse

  def initializeRandomState(size: Int): String = {
    val generator = new Random()
    val randomState = for (i <- 1 to size) yield generator.nextInt(2)
    return randomState.toList.mkString
  }

  def padBothSides(state: String): String = "0" + state + "0"

}

class CellularAutomaton(val initialState: String) {

  val allStates = ArrayBuffer[String](initialState)

  override def toString(): String = allStates.foldLeft("")(_ + "\n" + _)

  def applyRule(rule: Int): String = {
    val binaryRule = CellularAutomaton.ruleAsBinary(rule)
    val nextState = for (i <- (0 to initialState.length - 1)) yield {
      val triple = CellularAutomaton.padBothSides(allStates.last).drop(i).take(3)
      val tripleAsInt = Integer.parseInt(triple, 2)
      binaryRule(binaryRule.length - 1 - tripleAsInt)
    }
    allStates += nextState.mkString
    return nextState.mkString
  }

}

object CellularAutomatonTest extends App {

  // To initialize random initial state:
  // val automaton = new CellularAutomaton(CellularAutomaton.initializeRandomState)
  val automaton = new CellularAutomaton("0" * 50 + "1" + "0" * 50)
  val rule = 30
  println("THE AUTOMATON WITH ITS INITIAL STATE:")
  println(automaton)
  println("-------------------------------------")
  println("AFTER SOME STEPS:")
  for (i <- 1 to 100) automaton.applyRule(rule)
  println(automaton)

}
