package com.knoldus

import java.io.File

import akka.actor.{Actor, ActorSystem, Props}
import akka.util.Timeout

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import akka.pattern.ask


import scala.concurrent.duration._
import scala.io.Source

class LogAnalysisAkka extends FileBasicOperation with Actor{
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

  override def receive: Receive ={
    case msg:String=>val list=getListOfFile(msg)
       val re=Future{traverseFile(list,Map("error"->0,"warn"->0))}
      implicit val timeout = Timeout(14.seconds);
        val result =Await.result(re, timeout.duration);
        sender() ! "error is ->"+result("error") + " warn is ->"+result("warn")
   }
}

object LogAnalysisAkka extends App{

  val actorSystem=ActorSystem("ActorSystem")
  val actor = actorSystem.actorOf(Props[LogAnalysisAkka], "RootActor");
  implicit val timeout = Timeout(14.seconds);

  val future = actor ? "/home/knoldus/Downloads/Io2/";
  val result = Await.result(future, timeout.duration);
  println(result)

}


