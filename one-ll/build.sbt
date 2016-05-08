name := "one-ll"

version := "1.0"

scalaVersion := "2.11.8"

scalacOptions ++= Seq("-optimize", "-Ybackend:GenBCode", "-Yopt:l:classpath", "-Yopt-warnings:_")
