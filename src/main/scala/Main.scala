import scala.swing.FileChooser
import java.io.File

import scala.collection.mutable.ListBuffer

object Main {
  def main(args: Array[String]): Unit = {
    try {
      var SquareList  = readFile(choosePlainFile("Выбор Файла").get.toString);
      println("Производится поиск ответа(сложность n!)")
      if(TranspositionSolver.solve(SquareList, SquareList.length)>0) println("Задача решена") else println("Ответ не найден")
    } catch {
      case ioe: ArrayIndexOutOfBoundsException => println("Файл должен содержать 12 строк по 4 числа, раздленные пробелами")
      case e: Exception => println("Ожидается текстовый файл на входе")
    }
  }


  def readFile(filename: String): Array[Array[Int]] = {
    val bufferedSource = io.Source.fromFile(filename)
    val lines = (for (line <- bufferedSource.getLines()) yield line.split(" ").map(
      (s:String)=> {
        try {
          s.toInt
        } catch {
          case e: Exception => 0
        }
      }
    )).toArray
    bufferedSource.close
    lines
  }

  def choosePlainFile(title: String = ""): Option[File] = {
    val chooser = new FileChooser(new File("."))
    chooser.title = title
    val result = chooser.showOpenDialog(null)
    if (result == FileChooser.Result.Approve) {
      println("Выбран: " + chooser.selectedFile)
      Some(chooser.selectedFile)
    } else None
  }


}
