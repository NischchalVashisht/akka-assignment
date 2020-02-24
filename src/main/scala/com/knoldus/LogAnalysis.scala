package com.knoldus

import java.io.File

import scala.io.Source

class LogAnalysis extends FileBasicOperation{
  def readAndOperate(filename: File): Map[String,Option[Int]] = {
    val copyFileName = filename.toString

    val resultMap=Source.fromFile(s"$copyFileName").getLines().flatMap(_.split(" ")).toList.groupBy((word:String)=>word).mapValues(_.length)
    Map("error"->resultMap.get("[ERROR]"))++Map("warn"->resultMap.get("[WARN]"))
  }

  override def traverseFile(listOfFile: List[File],resultMap:Map[String,Int]): Map[String,Int] = {
    listOfFile match {
      case Nil => resultMap

      case head :: Nil if (head.isFile) => {
        val x: Map[String,Option[Int]] = readAndOperate(head)
        val temp1=resultMap("error")
        val temp2=resultMap("warn")
        resultMap ++ Map("error"->(x("error").getOrElse(0)+temp1) ) ++ Map("warn"->(x("warn").getOrElse(0)+temp2))
      }

      case head :: tail if (head.isFile) => {
        val x = readAndOperate(head)
        val temp1=resultMap("error")
        val temp2=resultMap("warn")
        traverseFile(tail, resultMap ++ Map("error"->(x("error").getOrElse(0)+temp1) ) ++ Map("warn"->(x("warn").getOrElse(0)+temp2)))
      }
    }
  }


}


