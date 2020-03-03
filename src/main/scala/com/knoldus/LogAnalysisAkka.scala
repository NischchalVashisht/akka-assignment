package com.knoldus

import java.io.File

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.util.Timeout
import com.typesafe.config.ConfigFactory

import scala.concurrent.duration._
import scala.io.Source

class LogAnalysisAkka extends FileBasicOperation with Actor with ActorLogging {
  def readAndOperate(filename: File): Map[String, Option[Int]] = {
    val copyFileName = filename.toString
    val source = Source.fromFile(s"$copyFileName")
    val resultMap = source.getLines().flatMap(_.split(" ")).toList.groupBy((word: String) => word).mapValues(_.length)
    source.close()
    Map("error" -> resultMap.get("[ERROR]")) ++ Map("warn" -> resultMap.get("[WARN]")) ++ Map("info" -> resultMap.get("[INFO]"))
  }

  override def traverseFile(listOfFile: List[File], resultMap: Map[String, Int]): Map[String, Int] = {
    listOfFile match {
      case Nil => resultMap

      case head :: Nil if head.isFile =>
        val x: Map[String, Option[Int]] = readAndOperate(head)
        val temp1 = resultMap("error")
        val temp2 = resultMap("warn")
        val temp3 = resultMap("info")
        resultMap ++ Map("error" -> (x("error").getOrElse(0) + temp1)) ++ Map("warn" -> (x("warn").getOrElse(0) + temp2)) ++ Map("info" -> (x("info").getOrElse(0) + temp3))
      case head :: tail if head.isFile =>
        val x = readAndOperate(head)
        val temp1 = resultMap("error")
        val temp2 = resultMap("warn")
        val temp3 = resultMap("info")
        traverseFile(tail, resultMap ++ Map("error" -> (x("error").getOrElse(0) + temp1)) ++ Map("warn" -> (x("warn").getOrElse(0) + temp2)) ++ Map("info" -> (x("info").getOrElse(0) + temp3)))
    }
  }

  override def receive: Receive = {
    case msg: String => val list = getListOfFile(msg)
      val result = traverseFile(list, Map("error" -> 0, "warn" -> 0, "info" -> 0))
      log.info("error is ->" + result("error") / 10 + " warn is ->" + result("warn") + "info is " + result("info"))
     case _ => log.info("default case")
  }
}

object LogAnalysisAkkaOb extends App  {
  val config = ConfigFactory.load()
  val system = ActorSystem("LogFilesActorSystem", config.getConfig("configuration"))
  val confStr="default-dispatcher"
  val threads=6

  val actorSystem = ActorSystem("ActorSystem")
  val actor = actorSystem.actorOf(Props[LogAnalysisAkka], "RootActor")
  implicit val timeout: Timeout = Timeout(14.seconds)

  val future = actor ! "/home/knoldus/Downloads/Io2/"
  actorSystem.terminate()
}


