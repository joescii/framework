package net.liftweb
package http
package js

import json._
import JE._
import DiffTransformers._
import org.specs2.mutable.Specification

object DiffTransformerExamples extends Specification{

  "Different int values example" in {
    val d:Diff  = JInt(1) diff JInt(2)
    val f:JsCmd = JsRaw("x = 2").cmd
    d.transformer("x").toJsCmd mustEqual(f.toJsCmd)
  }

  "Different value types example" in {
    val d:Diff  = JBool(true) diff JString("stuff")
    val f:JsCmd = JsRaw("x = \"stuff\"").cmd
    println(d)
    d.transformer("x").toJsCmd mustEqual(f.toJsCmd)
  }
}
