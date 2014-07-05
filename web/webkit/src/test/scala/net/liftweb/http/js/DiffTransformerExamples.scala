package net.liftweb
package http
package js

import json._
import JE._
import DiffTransformers._
import org.specs2.mutable.Specification

object DiffTransformerExamples extends Specification{
  "Same values" in {
    val d:Diff  = JString("same") diff JString("same")
    val f:JsCmd = JsCmds.Noop
    d.transformer(JsVar("x")) mustEqual(f)
  }

  "Different int values example" in {
    val d:Diff  = JInt(1) diff JInt(2)
    val f:JsCmd = JsRaw("x = 2").cmd
    d.transformer(JsVar("x")).toJsCmd mustEqual(f.toJsCmd)
  }

  "Different value types example" in {
    val d:Diff  = JBool(true) diff JString("stuff")
    val f:JsCmd = JsRaw("x = \"stuff\"").cmd
    d.transformer(JsVar("x")).toJsCmd mustEqual(f.toJsCmd)
  }

  "Append value to array" in {
    val d:Diff  = JArray(JBool(false) :: JInt(42) :: JString("yo") :: Nil) diff
      JArray(JBool(false) :: JInt(42) :: JString("yo") :: JInt(10) :: Nil )
    val f:JsCmd = JsRaw("x.push(10)").cmd
    d.transformer(JsVar("x")).toJsCmd mustEqual(f.toJsCmd)
  }

  "Append 2 values to array" in {
    val d:Diff  = JArray(JBool(false) :: JInt(42) :: Nil) diff
      JArray(JBool(false) :: JInt(42) :: JString("yo") :: JInt(10) :: Nil )
    val f:JsCmd = (
      JsRaw("x[x.indexOf(false)]=void 0").cmd &
      JsRaw("x.push(10)")
    .cmd)
    d.transformer(JsVar("x")).toJsCmd mustEqual(f.toJsCmd)
  }

  "Prepend 2 values to array" in {
    val d:Diff  = JArray(JBool(false) :: JInt(42) :: Nil) diff
      JArray(JString("yo") :: JInt(10) :: JBool(false) :: JInt(42) :: Nil )
    println(d)
    val f:JsCmd = (JsRaw("x.push(\"yo\")").cmd & JsRaw("x.push(10)").cmd)
    d.transformer(JsVar("x")).toJsCmd mustEqual(f.toJsCmd)
  }

//  "Remove first 2 values from array" in {
//    val d:Diff  =   JArray(JBool(false) :: JInt(42) :: JString("yo") :: JInt(10) :: Nil ) diff
//      JArray(JString("yo") :: JInt(10) :: Nil)
//    println(d)
//    val f:JsCmd = (JsRaw("x.push(false)").cmd & JsRaw("x.push(42)").cmd)
//    d.transformer(JsVar("x")).toJsCmd mustEqual(f.toJsCmd)
//  }
}
