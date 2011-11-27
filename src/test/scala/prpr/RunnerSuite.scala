package prpr

import org.scalatest.FunSuite
import scala.collection.mutable.Stack
 
class RunnerSuite extends FunSuite {
  test("runner can run the sample program") {
    val prpr = "あずにゃん"
    val runner = new Runner(prpr)
    runner.execute("あずにゃんペロﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛペロペロﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛペロﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛペロﾍﾟﾛあずにゃんペロあずにゃんあずにゃんﾍﾟﾛあずにゃんﾍﾟﾛﾍﾟﾛあずにゃんペロﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛペロペロﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛペロﾍﾟﾛペロペロﾍﾟﾛペロﾍﾟﾛあずにゃんペロあずにゃんあずにゃんﾍﾟﾛあずにゃんﾍﾟﾛﾍﾟﾛあずにゃんペロﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛペロペロﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛペロペロﾍﾟﾛペロﾍﾟﾛペロペロあずにゃんペロあずにゃんあずにゃんﾍﾟﾛあずにゃんﾍﾟﾛﾍﾟﾛあずにゃんペロﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛペロペロﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛペロﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛペロペロあずにゃんペロあずにゃんあずにゃんﾍﾟﾛあずにゃんﾍﾟﾛﾍﾟﾛあずにゃんペロﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛペロペロﾍﾟﾛﾍﾟﾛﾍﾟﾛﾍﾟﾛペロﾍﾟﾛﾍﾟﾛペロﾍﾟﾛﾍﾟﾛペロペロあずにゃんペロあずにゃんあずにゃんﾍﾟﾛあずにゃんﾍﾟﾛﾍﾟﾛあずにゃんペロペロペロペロペロペロペロペロペロペロﾍﾟﾛﾍﾟﾛﾍﾟﾛペロペロﾍﾟﾛペロあずにゃんペロあずにゃんあずにゃんﾍﾟﾛあずにゃんﾍﾟﾛﾍﾟﾛあずにゃんペロペロペロペロペロペロペロペロペロペロﾍﾟﾛﾍﾟﾛペロペロペロペロペロあずにゃんペロあずにゃんあずにゃんﾍﾟﾛあずにゃんﾍﾟﾛﾍﾟﾛあずにゃんペロペロペロペロペロペロペロペロペロペロﾍﾟﾛﾍﾟﾛペロペロﾍﾟﾛペロペロあずにゃんペロあずにゃんあずにゃんﾍﾟﾛあずにゃんﾍﾟﾛﾍﾟﾛあずにゃんペロペロペロペロペロペロペロペロペロペロﾍﾟﾛﾍﾟﾛﾍﾟﾛペロペロﾍﾟﾛペロあずにゃんペロあずにゃんあずにゃんﾍﾟﾛあずにゃんﾍﾟﾛﾍﾟﾛあずにゃんペロペロペロペロペロペロペロペロペロペロﾍﾟﾛﾍﾟﾛペロペロペロペロペロあずにゃんペロあずにゃんあずにゃんﾍﾟﾛあずにゃんﾍﾟﾛﾍﾟﾛあずにゃんペロペロペロペロペロペロペロペロペロペロﾍﾟﾛﾍﾟﾛペロペロﾍﾟﾛペロペロあずにゃんペロあずにゃんあずにゃんﾍﾟﾛあずにゃんﾍﾟﾛﾍﾟﾛ")
    runner.execute("あずにゃんペロﾍﾟﾛあずにゃんあずにゃんペロﾍﾟﾛあずにゃんペロペロあずにゃんあずにゃんペロペロﾍﾟﾛペロﾍﾟﾛあずにゃんあずにゃんペロペロあずにゃんペロペロあずにゃんﾍﾟﾛあずにゃんあずにゃんﾍﾟﾛあずにゃんあずにゃんペロﾍﾟﾛあずにゃんペロペロペロあずにゃんﾍﾟﾛあずにゃんペロあずにゃんあずにゃんペロあずにゃんペロペロあずにゃんペロﾍﾟﾛあずにゃんあずにゃんあずにゃんペロﾍﾟﾛあずにゃんペロペロあずにゃんあずにゃんペロﾍﾟﾛあずにゃんあずにゃんペロペロあずにゃんペロペロペロあずにゃんペロペロあずにゃんペロﾍﾟﾛあずにゃんペロあずにゃんﾍﾟﾛあずにゃんあずにゃんペロペロあずにゃんペロペロあずにゃんペロﾍﾟﾛあずにゃんペロﾍﾟﾛペロペロﾍﾟﾛあずにゃんﾍﾟﾛﾍﾟﾛﾍﾟﾛ")
  }
}
