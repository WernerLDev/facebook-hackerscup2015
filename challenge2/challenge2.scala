import scala.io.Source._

case class Food(p:Int, c:Int, f:Int) {
        override def toString():String = {
            "{P: " + p + " C: " + c + " F: " + f + "} "
        }
}

case class TestCase(p:Int, c:Int, f:Int, food:List[Food]) {
    
    val validFood = food.filter(x => x.p <= p && x.c <= c && x.f <= f)
    
    def getCombinations(size:Int):List[List[Food]] = {
        if(size <= food.length) food.combinations(size).toList ++ getCombinations(size + 1)
        else Nil
    }
    
    def countNutrients(f:List[Food]):(Int, Int, Int) = f match {
        case x :: xs => val next = countNutrients(xs)
                        (x.p + next._1, x.c + next._2, x.f + next._3)
        case Nil => (0,0,0)
    }
    
    @scala.annotation.tailrec
    final def isEatable(e:List[List[Food]]):Boolean = e match {
        case x :: xs => {
                            val counted = countNutrients(x)
                            if(counted._1 == p && counted._2 == c && counted._3 == f){
                                true
                            } else isEatable(xs)
                         }
        case Nil => false
                         
    }
    
    def findEatable(size:Int):Boolean = {
        if(isEatable(validFood.combinations(size).toList)) true
        else if(size < validFood.length) findEatable(size + 1)
        else false
    }
    
    def isPossibleToEat():Boolean = findEatable(1)
    
    override def toString():String = {
        "Max p: " + p + " max c: " + c + " max f: " + f + "  \n" +
        "Food: " + food.toList + "\n\n"
    }
}

object Main {

    def getFoodLines(lines:List[List[String]], count:Int):List[Food] = lines match {
        case x :: xs => if(count > 0){
                            val p = x.lift(0).getOrElse("0").toInt
                            val c = x.lift(1).getOrElse("0").toInt
                            val f = x.lift(2).getOrElse("0").toInt
                            Food(p,c,f) :: getFoodLines(xs, count - 1)
                        } else Nil
        case Nil => Nil
    }

    def createDatastructure(lines:List[List[String]]):List[TestCase] = lines match {
        case x :: xs =>
                        val p = x.lift(0).getOrElse("0").toInt
                        val c = x.lift(1).getOrElse("0").toInt
                        val f = x.lift(2).getOrElse("0").toInt
                        val count = xs.head.head.toInt
                        val food = getFoodLines(xs.tail, count)
                        TestCase(p,c,f, food) :: createDatastructure(xs.tail.drop(food.length))
        case Nil => Nil
    }

    val input = fromFile("input.txt").getLines.map(_.split(' ').toList).toList
    val count = input.head.head.toInt
    val data = createDatastructure(input.tail)

     def writeOutput(c:List[TestCase], count:Int):Unit = c match {
        case x::xs => val canEat = x.isPossibleToEat()
                      val eatOutput = if(canEat == true) "yes" else "no"
                      println("Case #" + count + ": " + eatOutput)
                      writeOutput(xs, count+1)
        case Nil => 
    }

    def main(args:Array[String]) {
        
        writeOutput(data, 1)    

    }

}
