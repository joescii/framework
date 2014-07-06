package net.liftweb
package http
package js

import json._
import JE._
import JsonDeltaFuncs._
import org.specs2.mutable.Specification
import net.liftweb.http.js.JsCmds.JsFor

object JsonDeltaFuncExamples extends Specification{
  "Same values" in {
    val dfn = JString("same") dfn JString("same")
    val jsf = JsCmds.Noop
    dfn(JsVar("x")) mustEqual(jsf)
  }

  "Different int values example" in {
    val dfn = JInt(1) dfn JInt(2)
    val jsf = JsRaw("x = 2").cmd
    dfn(JsVar("x")).toJsCmd.trim mustEqual(jsf.toJsCmd.trim)
  }

  "Different value types example" in {
    val dfn = JBool(true) dfn JString("stuff")
    val jsf = JsRaw("x = \"stuff\"").cmd
    dfn(JsVar("x")).toJsCmd.trim mustEqual(jsf.toJsCmd.trim)
  }

  "Append value to array" in {
    val dfn = JArray(JBool(false) :: JInt(42) :: JString("yo") :: Nil) dfn
      JArray(JBool(false) :: JInt(42) :: JString("yo") :: JInt(10) :: Nil )
    val jsf = JsRaw("x[3] = 10").cmd
    dfn(JsVar("x")).toJsCmd.trim mustEqual(jsf.toJsCmd.trim)
  }

  "Append 2 values to array" in {
    val dfn = JArray(JBool(false) :: JInt(42) :: Nil) dfn
      JArray(JBool(false) :: JInt(42) :: JString("yo") :: JInt(10) :: Nil )
    val jsf = (
      JsRaw("x[2] = \"yo\"").cmd &
      JsRaw("x[3] = 10").cmd
    )
    dfn(JsVar("x")).toJsCmd.trim mustEqual(jsf.toJsCmd.trim)
  }

  "Change value in array" in {
    val dfn = JArray(JBool(false) :: JInt(42) :: JString("yo") :: JInt(10) :: Nil) dfn
      JArray(JBool(false) :: JInt(42) :: JString("dawg") :: JInt(10) :: Nil )
    val jsf = JsRaw("x[2] = \"dawg\"").cmd
    dfn(JsVar("x")).toJsCmd.trim mustEqual(jsf.toJsCmd.trim)
  }

  // Not optimal, but whatever...
  "Prepend 2 values to array" in {
    val dfn = JArray(JBool(false) :: JInt(42) :: Nil) dfn
      JArray(JString("yo") :: JInt(10) :: JBool(false) :: JInt(42) :: Nil )
    val jsf = (
      JsRaw("x[0] = \"yo\"").cmd &
      JsRaw("x[1] = 10").cmd &
      JsRaw("x[2] = false").cmd &
      JsRaw("x[3] = 42").cmd
    )
    dfn(JsVar("x")).toJsCmd.trim mustEqual(jsf.toJsCmd.trim)
  }

  "Remove first 2 values from array" in {
    val dfn = JArray(JBool(false) :: JInt(42) :: JString("yo") :: JInt(10) :: Nil ) dfn
      JArray(JString("yo") :: JInt(10) :: Nil)
    val jsf = (
      JsRaw("x[0] = \"yo\"").cmd &
      JsRaw("x[1] = 10").cmd &
      JsFor(JsRaw("i=0"),JsLt(JsVar("i"),JInt(2)),JsRaw("i++"),Call("x.pop"))
    )
    dfn(JsVar("x")).toJsCmd.trim mustEqual(jsf.toJsCmd.trim)
  }

  "Change an existing field in an obj" in {
    val x = parse("""
    {
      "lang": "scala",
      "year": 2006
    }""")
    val y = parse("""
    {
      "lang": "haskell",
      "year": 2006
    }""")
    val dfn = x dfn y
    val jsf = JsRaw("x[\"lang\"] = \"haskell\"").cmd

    dfn(JsVar("x")).toJsCmd.trim mustEqual(jsf.toJsCmd.trim)
  }

  "Add a field to an obj" in {
    val x = parse("""
    {
      "lang": "scala",
      "year": 2006
    }""")
    val y = parse("""
    {
      "lang": "scala",
      "year": 2006,
      "versions": ["2.9.2","2.10","2.11"]
    }""")
    val dfn = x dfn y
    val jsf = JsRaw("x[\"versions\"] = [\"2.9.2\",\"2.10\",\"2.11\"]").cmd

    dfn(JsVar("x")).toJsCmd.trim mustEqual(jsf.toJsCmd.trim)
  }

}
