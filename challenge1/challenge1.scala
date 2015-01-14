import scala.io.Source._

case class TestCase(number:String) {
    
    val numbers:List[Char] = number.toList
    
    def swapChars(chars:List[Char], c1:Char, c2:Char) : List[Char] = chars match {
        case x :: xs => if     (x == c1) c2 :: swapChars(xs, c1, c2) 
                        else if(x == c2) c1 :: swapChars(xs, c1, c2)
                        else              x :: swapChars(xs, c1, c2)
        case Nil => Nil
    }
    
    def lowest():String = {
        val lowestChar:Char = if(numbers.sortBy(_.toInt).filter(_ != '0').length > 0) {
            numbers.sortBy(_.toInt).filter(_ != '0').head
        } else { numbers.head }
        swapChars(numbers, numbers.head, lowestChar).mkString
    }
    
    def highest():String = {
        val highestChar:Char = numbers.sortBy(_.toInt).reverse.head
        swapChars(numbers, numbers.head, highestChar).mkString
    }

}

object Main {

    val input = fromFile("input.txt").getLines.toList
               
    val numberOfCases = input.head

    def writeOutput(c:List[TestCase], count:Int):Unit = c match {
        case x::xs => println("Case #" + count + ": " + x.lowest + " " + x.highest)
                      writeOutput(xs, count+1)
        case Nil => 
    }

    def main(args:Array[String]) {
        
        val cases = input.tail.map(x => TestCase(x))
        writeOutput(cases, 1)
        
    }

}
