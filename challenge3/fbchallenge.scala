import scala.io.Source._

sealed abstract class Move
case object Left  extends Move
case object Right extends Move
case object Up    extends Move
case object Down  extends Move

case class Pos(row:Int, col:Int, steps:Int)

case class Maze(maze:Vector[Vector[Char]]) {

    val startPos = findChar('S')
    val goal = findChar('G')
    
    val turretChars = List('<', '^', '>', 'v')
    val turrets:List[(Char, Pos)] = {
        def findTurret(row:Int, col:Int):List[(Char, Pos)] = {
            if(row < maze.length) {
                if(col < maze(row).length) {
                    val c = maze(row)(col)
                    if(turretChars.contains(c)) {
                        (c, Pos(row, col, 0)) :: findTurret(row, col + 1)
                    } else findTurret(row, col + 1)
                } else findTurret(row + 1, 0)
            } else Nil
        }
        findTurret(0,0)
    }
    final def deadlyList(step:Int) = turrets.map(x => findDeadlyPositions(x, step)).flatten
    val deadly = Vector(0,1,2,3).map(x => deadlyList(x))
    
    final def getTurret(c:Char):Int = {
        if(c=='>') 0
        else if(c=='v') 1
        else if(c == '<') 2
        else if(c == '^') 3
        else -1
    }
    
    def getRotatedTurret(t:Int, step:Int) = if(t + step > 3) (t + step) - 4 else t + step
    
    final def findDeadlyPositions(turret:(Char, Pos), step:Int):List[Pos] = {
        val t = getRotatedTurret( getTurret(turret._1), step )
        val next = if(t == 0) Pos(turret._2.row, turret._2.col + 1, step)
                   else if(t == 1) Pos(turret._2.row + 1, turret._2.col, step)
                   else if(t == 2) Pos(turret._2.row, turret._2.col - 1, step)
                   else if(t == 3) Pos(turret._2.row - 1, turret._2.col, step)
                   else Pos(-1,-1,step)
        val c = getChar(next)
        if(c == '.' || c == 'S' || c == 'G') next::findDeadlyPositions( (turret._1, next), step )
        else Nil
    }
    
    final def findChar(c:Char):Pos = {
        val row = maze.filter(x => x.contains(c))
        Pos(maze.indexOf(row.head), row.head.indexOf(c), 0)
    }
    
    final def getChar(p:Pos):Char = {
        if(p.row >= 0 && p.row < maze.length) {
            val cols = maze(p.row)
            if(p.col >= 0 && p.col < cols.length) {
                cols(p.col)
            } else '@'
        } else '@'
    }
    
    final def isDeadly(p:Pos):Boolean = deadly(p.steps).contains(p)
    
    final def isValid(p:Pos):Boolean = {
        if(p.row >= 0 && p.row < maze.length) {
            val xpos = maze(p.row)
            if(p.col >= 0 && p.col < xpos.length) {
                val c = xpos(p.col)
                if(c == '#' || c == '>' || c == '<' || c == '^' || c == 'v') false
                else isDeadly(p) == false
            } else false
        } else false
    }
    
    def neighbors(p:Pos):List[(Pos, Move)] = {
        val nextStep = if(p.steps == 3) 0 else p.steps + 1
        List(
         (Pos(p.row, p.col - 1, nextStep), Left),
         (Pos(p.row, p.col + 1, nextStep), Right),
         (Pos(p.row - 1, p.col, nextStep), Up),
         (Pos(p.row + 1, p.col, nextStep), Down)
        ).filter(x => isValid(x._1))
    }
    
}

case class Solver(level:Maze) {

    val startPos = level.startPos
    val goal = level.goal
    
    def done(p:Pos):Boolean = p.row == goal.row && p.col == goal.col
    
    final def newNeighbors(p:Pos, history:Set[Pos], steps:Int):Stream[(Pos, Int)] = {
        level.neighbors(p).filter(x => history.contains(x._1) == false).map(x => (x._1, steps + 1)).toStream
    }
    
    def getPaths(initial:Stream[(Pos,Int)], explored:Set[Pos]):Stream[(Pos, Int)] = {
        if(initial.isEmpty) Stream.Empty
        else {
            val more = for {
                block <- initial
                neighbors <- newNeighbors(block._1, explored, block._2)
            } yield neighbors
            initial #::: getPaths(more, explored ++ more.map(x => x._1))
        }
    }
    
    lazy val solutions = getPaths(Stream((startPos, 0)), Set()) filter (x => done(x._1))
}


object Main {

    def main(args:Array[String]) {
        if(args.length > 0) {
            val t1 = System.currentTimeMillis
            val input = fromFile(args.head).getLines.toList
            
            lazy val vector:Vector[Vector[Char]] = 
            Vector(input.map(str => Vector(str: _*)): _*)
            
            val maze = Maze(vector)
            val solver = Solver(maze)

            val solution = solver.solutions.head
            println("Solution found in " + solution._2 + " steps")
            val t2 = System.currentTimeMillis
            println("Executiontime: " + (t2-t1) + " msecs")
        } else {
            val t1 = System.currentTimeMillis

            val testlevel = """..#v.......#...#....v...#..v...##........#...........#....##.#.............#...
                               |#.S.v.......#..........#.#......##..........>.......>##..#...<..........<...#.#
                               |.....#....#............>.#......^......#.#...#.#<........v....#..#.....#......>
                               |....#..#^........#>......>......#.#.....>................>..#.....#.......#<...
                               |.....#........#............v.^.#...^.....................<.....#........#.#.#..
                               |.<..#.........<#.#......#......#.......................v.>........>.....#....#.
                               |<.....#..........#....>.....#......>...............#......##......##.......#...
                               |........<.....v...v.#...#....#.............v......<...#............#.<....#...<
                               |^...............#<.#...#^.#...#...#..#.....#..................#.#....#....#>>v.
                               |..........#...##....................^..^.#.........#........^...........#......
                               |........^.....#.##..#..^........#.........#v.v..#............<#..G...^........#""".stripMargin

            lazy val vector:Vector[Vector[Char]] = 
                Vector(testlevel.split("\n").map(str => Vector(str: _*)): _*)
                
            val maze = Maze(vector)
            val solver = Solver(maze)
            println("Solution found in " + solution._2 + " steps")
            val t2 = System.currentTimeMillis
            println("Executiontime: " + (t2-t1) + " msecs")
        }
    }
    
}

