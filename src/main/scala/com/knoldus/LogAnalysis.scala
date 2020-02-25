package com.knoldus

import java.io.File

import scala.io.Source

class LogAnalysis extends FileBasicOperation{
  def readAndOperate(filename: File): Map[String,Option[Int]] = {
    val copyFileName = filename.toString
    val source=Source.fromFile(s"$copyFileName")
    val resultMap=source.getLines().flatMap(_.split(" ")).toList.groupBy((word:String)=>word).mapValues(_.length)
    source.close()
    Map("error"->resultMap.get("[ERROR]"))++Map("warn"->resultMap.get("[WARN]"))++Map("info"->resultMap.get("[INFO]"))

  }

  override def traverseFile(listOfFile: List[File],resultMap:Map[String,Int]): Map[String,Int] = {
    listOfFile match {
      case Nil => resultMap

      case head :: Nil if head.isFile =>
        val x: Map[String,Option[Int]] = readAndOperate(head)
        val temp1=resultMap("error")
        val temp2=resultMap("warn")
        val temp3=resultMap("info")
        resultMap ++ Map("error"->(x("error").getOrElse(0) + temp1) ) ++ Map("warn"->(x("warn").getOrElse(0) + temp2)) ++ Map("info"->(x("info").getOrElse(0) + temp3))

      case head :: tail if head.isFile =>
        val x = readAndOperate(head)
        val temp1=resultMap("error")
        val temp2=resultMap("warn")
        val temp3=resultMap("info")
        traverseFile(tail, resultMap ++ Map("error"->(x("error").getOrElse(0) + temp1) ) ++ Map("warn"->(x("warn").getOrElse(0) + temp2)) ++ Map("info"->(x("info").getOrElse(0) + temp3)))
    }
  }


}


