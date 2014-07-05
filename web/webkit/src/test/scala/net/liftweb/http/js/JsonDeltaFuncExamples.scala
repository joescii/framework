package net.liftweb
package http
package js

import json._
import JE._
import JsonDeltaFuncs._
import org.specs2.mutable.Specification

object JsonDeltaFuncExamples extends Specification{
  "Same values" in {
    val dfn = JString("same") dfn JString("same")
    val jsf = JsCmds.Noop
    dfn(JsVar("x")) mustEqual(jsf)
  }

  "Different int values example" in {
    val dfn = JInt(1) dfn JInt(2)
    val jsf = JsRaw("x = 2").cmd
    dfn(JsVar("x")).toJsCmd mustEqual(jsf.toJsCmd)
  }

  "Different value types example" in {
    val dfn = JBool(true) dfn JString("stuff")
    val jsf = JsRaw("x = \"stuff\"").cmd
    dfn(JsVar("x")).toJsCmd mustEqual(jsf.toJsCmd)
  }

  "Append value to array" in {
    val dfn = JArray(JBool(false) :: JInt(42) :: JString("yo") :: Nil) dfn
      JArray(JBool(false) :: JInt(42) :: JString("yo") :: JInt(10) :: Nil )
    val jsf = JsRaw("x[3] = 10").cmd
    dfn(JsVar("x")).toJsCmd mustEqual(jsf.toJsCmd)
  }

  "Append 2 values to array" in {
    val dfn = JArray(JBool(false) :: JInt(42) :: Nil) dfn
      JArray(JBool(false) :: JInt(42) :: JString("yo") :: JInt(10) :: Nil )
    val jsf = (
      JsRaw("x[2] = \"yo\"").cmd &
      JsRaw("x[3] = 10")
    .cmd)
    dfn(JsVar("x")).toJsCmd mustEqual(jsf.toJsCmd)
  }

//  "Prepend 2 values to array" in {
//    val dfn = JArray(JBool(false) :: JInt(42) :: Nil) dfn
//      JArray(JString("yo") :: JInt(10) :: JBool(false) :: JInt(42) :: Nil )
//    println(dfn)
//    val jsf = (JsRaw("x.push(\"yo\")").cmd & JsRaw("x.push(10)").cmd)
//    dfn(JsVar("x")).toJsCmd mustEqual(jsf.toJsCmd)
//  }

//  "Remove first 2 values from array" in {
//    val dfn =   JArray(JBool(false) :: JInt(42) :: JString("yo") :: JInt(10) :: Nil ) dfn
//      JArray(JString("yo") :: JInt(10) :: Nil)
//    println(dfn)
//    val jsf = (JsRaw("x.push(false)").cmd & JsRaw("x.push(42)").cmd)
//    dfn(JsVar("x")).toJsCmd mustEqual(jsf.toJsCmd)
//  }
}
