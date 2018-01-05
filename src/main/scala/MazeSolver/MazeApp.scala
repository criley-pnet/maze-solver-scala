package MazeSolver

import scala.collection.mutable
import scala.io.Source

object MazeApp extends App {

  val fileName = "maze.txt"
  val fileSrc = Source.fromResource(fileName)
  try {
    val lines = fileSrc
      .getLines
      .toArray

    lines.foreach(println)

    val problem = new Problem(lines)
    val solver = new Solver(problem)
    solver.solve()
    //val solver = new MazeSolver(map)

  } finally {
    fileSrc.close()
  }

}

class Node(val row:Int, val col:Int) {

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case obj: Node => obj.isInstanceOf[Node] && this.row == obj.row && this.col == obj.col
      case _ => false
    }
  }

  override def toString: String = s"($row, $col)"
}

class Problem(lines:Array[String]) {
  val wall = "#"

  val joinedLines = lines.mkString
  val lineLength = lines(0).length

  lines.foreach(l => assert(lineLength == l.length, "Maze must have same length of every line"))

  assert (joinedLines.indexOf("A") != -1 && joinedLines.indexOf("A") == joinedLines.lastIndexOf("A"), "Maze must have exactly one start")
  assert (joinedLines.indexOf("B") != -1 && joinedLines.indexOf("B") == joinedLines.lastIndexOf("B"), "Maze must have exactly one finish")
  println("Validation complete :)")

  def getStart(): Node = {
    val index = joinedLines.indexOf("A")

    val start = new Node(index / lineLength, index % lineLength)

    println(s"Maze starts at $start")

    start
  }

  def isFinish(node: Node): Boolean = {
    println(s"checking for solution at $node")
    lines(node.row)(node.col) == "B"
  }

  def getNextNodes(current: Node): Set[Node] = {
    val neighbors = Set(
      getNodeValue(current.row - 1, current.col),
      getNodeValue(current.row + 1, current.col),
      getNodeValue(current.row, current.col - 1),
      getNodeValue(current.row, current.col + 1)
    ).collect{case Some(i) => i}

    println(s"Neighbors are $neighbors")

    neighbors
  }

  def getNodeValue(row:Int, col:Int): Option[Node] = {
    if (row < 0 || col < 0 || row > lines.length || col > lineLength) {
      println("out of bounds")
      return None
    } else {
      if (lines(row)(col).toString == wall) {
        println("wall")
        return None
      } else {
        println("not wall")
        return Some(new Node(row, col))
      }
    }
  }
}

class Solver(problem: Problem) {
  def solve(): Unit = {
    var currentSet = new mutable.Queue[Node]()
    currentSet.enqueue(problem.getStart())

    var checkedSet = Set.empty[Node]

    while (currentSet.size > 0) {

      val parentNode = currentSet.dequeue()
      if (problem.isFinish(parentNode)) {
        println(s"Solved!!! $parentNode")
        return
      }

      for (elem <- problem.getNextNodes(parentNode)) {
        //Skip nodes already checked to prevent looping
        if (checkedSet.contains(elem)) {
          println(s"already tried $elem")
        } else {
          println(s"adding node to queue $elem")
          currentSet.enqueue(elem)
        }
      }

      println(s"adding to checked set $parentNode")
      checkedSet += parentNode
    }

    println(s"No solution.")
  }
}
