package com.knoldus

object MainOperating extends  App{
  val logAnalysis=new LogAnalysis
  val listOfFile= logAnalysis.getListOfFile("/home/knoldus/Downloads/Io2/")
  println(logAnalysis.getResult(listOfFile))
}

