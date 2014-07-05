package net.liftweb
package http
package js

import json._
import JE._
import JsonDeltaFuncs._
import org.specs2.mutable.Specification

object JsonDeltaFuncExamples extends Specification{
  "Same values" in {
    val d = JString("same") dfn JString("same")
    val f = JsCmds.Noop
    d(JsVar("x")) mustEqual(f)
  }

  "Different int values example" in {
    val d = JInt(1) dfn JInt(2)
    val f = JsRaw("x = 2").cmd
    d(JsVar("x")).toJsCmd mustEqual(f.toJsCmd)
  }

  "Different value types example" in {
    val d = JBool(true) dfn JString("stuff")
    val f = JsRaw("x = \"stuff\"").cmd
    d(JsVar("x")).toJsCmd mustEqual(f.toJsCmd)
  }

  "Append value to array" in {
    val d = JArray(JBool(false) :: JInt(42) :: JString("yo") :: Nil) dfn
      JArray(JBool(false) :: JInt(42) :: JString("yo") :: JInt(10) :: Nil )
    val f = JsRaw("x.push(10)").cmd
    d(JsVar("x")).toJsCmd mustEqual(f.toJsCmd)
  }

  "Append 2 values to array" in {
    val d = JArray(JBool(false) :: JInt(42) :: Nil) dfn
      JArray(JBool(false) :: JInt(42) :: JString("yo") :: JInt(10) :: Nil )
    val f = (
      JsRaw("x[x.indexOf(false)]=void 0").cmd &
      JsRaw("x.push(10)")
    .cmd)
    d(JsVar("x")).toJsCmd mustEqual(f.toJsCmd)
  }

  "Prepend 2 values to array" in {
    val d = JArray(JBool(false) :: JInt(42) :: Nil) dfn
      JArray(JString("yo") :: JInt(10) :: JBool(false) :: JInt(42) :: Nil )
    println(d)
    val f = (JsRaw("x.push(\"yo\")").cmd & JsRaw("x.push(10)").cmd)
    d(JsVar("x")).toJsCmd mustEqual(f.toJsCmd)
  }

//  "Remove first 2 values from array" in {
//    val d =   JArray(JBool(false) :: JInt(42) :: JString("yo") :: JInt(10) :: Nil ) dfn
//      JArray(JString("yo") :: JInt(10) :: Nil)
//    println(d)
//    val f = (JsRaw("x.push(false)").cmd & JsRaw("x.push(42)").cmd)
//    d(JsVar("x")).toJsCmd mustEqual(f.toJsCmd)
//  }
}
