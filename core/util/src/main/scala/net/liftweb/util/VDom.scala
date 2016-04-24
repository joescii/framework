package net.liftweb.util

import net.liftweb.json._

import scala.annotation.tailrec
import scala.xml._

object VDom {
  import VDomHelpers._
  case class VNode(tag:String, attributes:Map[String, String] = Map(), children:List[VNode] = List(), text:Option[String] = None)

  trait VNodeTransform
  case class VNodeInsert(position:Int, node:VNode) extends VNodeTransform
  case class VNodeDelete(position:Int) extends VNodeTransform
  case class VNodeReorder(before:Int, after:Int) extends VNodeTransform

  case class VNodeTransformTree(transforms:List[VNodeTransform], children:List[VNodeTransformTree])

  object typeHints extends TypeHints {
    val classToHint:Map[Class[_], String] = Map(
      classOf[VNodeInsert] -> "insert",
      classOf[VNodeDelete] -> "delete",
      classOf[VNodeReorder] -> "reorder"
    )
    val hint2Class:Map[String, Class[_]] = classToHint.map { case (c, h) => h -> c }.toMap
    override val hints: List[Class[_]] = classToHint.keysIterator.toList
    override def hintFor(clazz: Class[_]):String = classToHint(clazz)
    override def classFor(hint: String) = hint2Class.get(hint)
  }
  val formats = new Formats {
    override val dateFormat: DateFormat = DefaultFormats.lossless.dateFormat
    override val typeHints = VDom.typeHints
    override val typeHintFieldName = "type"
  }

  def diff(a:Node, b:Node):VNodeTransformTree = {

    val aChildren = a.nonEmptyChildren.filter(isntWhitespace).toList // before
    val bChildren = b.nonEmptyChildren.filter(isntWhitespace).toList // after

    val additions = if (aChildren.size < bChildren.size) {
      bChildren.diff(aChildren).map {
        case c => VNodeInsert(bChildren.indexOf(c), VNode.fromXml(c))
      }
    }
    else Nil

    val deletions = if (aChildren.size > bChildren.size) {
      aChildren.diff(bChildren).map {
        case c => /*println(c + " Deletions: index " + aChildren.indexOf(c)); */VNodeDelete(aChildren.indexOf(c))
      }
    }
    else Nil

    val reorderings = if (aChildren.size == bChildren.size) {
      aChildren.zip(bChildren).collect {
        case (ca, cb) if aChildren.indexOf(ca) != bChildren.indexOf(ca) &&
                        (aChildren.indexOf(ca) != -1 && bChildren.indexOf(ca) != -1)
                        => VNodeReorder(aChildren.indexOf(ca), bChildren.indexOf(ca))
        case _ => VNodeReorder(0, 0)
      }
    }
    else Nil

    val testList = if(aChildren.size == bChildren.size) {
      aChildren.zip(bChildren).collect {
        case (ca, cb) => bChildren.indexOf(ca)
        case _ => -1
      }
    }
    else Nil

    val reorderTest = sort(testList.filterNot(_ == -1).toArray).filterNot(_ == -1).map(a => VNodeReorder(a, a-1))

    println("SORT: " + sort(testList.filterNot(_ == -1).toArray))
    println("FINGERS CROSSED: " + reorderTest)

    /*val testList = if (aChildren.size == bChildren.size) {
      aChildren.zip(bChildren).collect {
        case (ca, cb) if aChildren.indexOf(ca) != -1 && bChildren.indexOf(ca) != -1 => VNodeReorder(aChildren.indexOf(cb), 0)
        case _ => VNodeReorder(-1,-1)
      }
    }
    else Nil

    val reorderingsTest = testList.reverse.filterNot(_ == VNodeReorder(-1,-1))
    val reorderTest2 = reorderingsTest.map {
      case VNodeReorder(a, 0) => VNodeReorder(a+(reorderingsTest.length-a), 0)
    }*/

    //println("aChildren: " + aChildren)
    //println("bChildren: " + bChildren)
    //println("zip: " + aChildren.zip(bChildren))
    //println("DIFF: " + aChildren.diff(bChildren))
    println("REORDERINGS: " + reorderings)
    //print("FIRST: " + reorderingsTest)
    //print("SECOND: " + reorderTest2)
    //println("Reorderings contains: " + reorderings.contains(VNodeReorder(0,0)))
    //println("******************** SORT RESULT: " + sort(List(4,3,2,1)))

    // Possibily List((0,0)).sort get index of value after sort??


    val transforms = //if (reorderings.contains(VNodeReorder(0,0))) {
      additions ++ deletions ++ reorderTest //reorderings.filterNot(_ == VNodeReorder(0,0))
    //}
    //else additions ++ deletions ++ reorderings

    val children = aChildren.zip(bChildren)
      .collect {
        case (ca, cb) if ca != cb => diff(ca, cb)     // This != check probably would benefit from memoizing
        case _ => VNodeTransformTree(List(), List())  // No changes for this node, make a placeholder
      }

    VNodeTransformTree(transforms, children)
  }

//  def sort(input: List[Int]):List[Int] = {
//    if (input != Nil && input.tail != Nil) {
//      if (input.head > input.tail.head) {
//        println("INSIDE SORT: " + input.head + " " + input.tail.head)
//        sort(List(input.tail.head, input.head):::input.tail.tail)
//      }
//      else {
//        val sortResult = sort(input.tail)
//        println("INSIDE SORT 2: " + sortResult)
//        if (input.head > sortResult.head) {
//          println("INSIDE SORT 3: " + sortResult.head + " " + input.head)
//          sort(List(sortResult.head, input.head) ::: sortResult.tail)
//        }
//        else
//          List(input.head) ::: sortResult
//      }
//    }
//    else input
//  }

  def sort(input: Array[Int]): List[Int] = {
    var changes = List(-1)
    val limit = input.length - 1
    for (a <- 1 to limit) {
      for (b <- limit to a by -1) {
        if (input(b) < input(b-1)) { // need to get b value anytime in this loop!
          changes = changes:::List(b)
          val x = input(b)
          input(b) = input(b - 1)
          input(b - 1) = x
        }
      }
    }
    changes
  }

  def compare(a:Node, b:Node):Float =
    if(a == b) 1f
    else 0f

  object VNode {
    def text(t:String):VNode = VNode("#text", Map(), List(), Some(t))
    def fromXml(n:Node):VNode = {
      if(n.label == pcdata) text(n.text)
      else {
        val attrs:Map[String, String] = n.attributes
          .collect { case UnprefixedAttribute(k, Text(v), _) => k -> v }
          .toMap
        val children:List[VNode] = n.nonEmptyChildren
          .filter(isntWhitespace)
          .map(fromXml)
          .toList

        VNode(n.label, attrs, children)
      }
    }
  }

  object VDomHelpers extends VDomHelpers
  trait VDomHelpers {
    val pcdata = "#PCDATA"
    def isText(n:Node) = n.label == pcdata
    def isWhitespace(n:Node)   = isText(n) && n.text.trim.isEmpty
    def isntWhitespace(n:Node) = !isWhitespace(n)

    def node(child:VNodeTransformTree*):VNodeTransformTree = VNodeTransformTree(List(), child.toList)
    def text(t:String) = VNode(pcdata, Map(), List(), Some(t))

    implicit class EnhancedVNodeTransformTree(t:VNodeTransformTree) {
      def withTransforms(transform:VNodeTransform*) = t.copy(transforms = transform.toList)
    }

    def nodeCount(n:Node):Int = {
      @tailrec
      def rec(ns:List[Node], acc:Int):Int = ns match {
        case n :: rest => rec(rest ++ n.nonEmptyChildren.filter(isntWhitespace), acc + 1)
        case Nil => acc
      }

      rec(List(n), 0)
    }

    def recFilter(n:Node, pred:Node => Boolean):Node = n match {
      case Elem(prefix, label, attributes, scope, children @ _*) =>
        Elem(prefix, label, attributes, scope, true, children.filter(pred).map(recFilter(_, pred)):_*)
      case _ => n
    }

    def insertNode(root:Node, newChild:Node, atIndex:Int, after:Boolean):Node = {
      def rec(parent:Elem, siblingsBefore:List[Node], child:Node, siblingsAfter:List[Node], index:Int):(Node, Int) =
        (parent, child, child.nonEmptyChildren.filter(isntWhitespace).toList, siblingsAfter) match {
          // We found our node by index, so insert
          case (Elem(prefix, label, attributes, scope, _ @ _*), _, _, _) if index == atIndex =>
            (Elem(prefix, label, attributes, scope, true, siblingsBefore ++ (child :: newChild :: siblingsAfter):_*), -1)

          // Child itself has children
          case (Elem(prefix, label, attributes, scope, _ @ _*), e:Elem, first :: rest, _) =>
            val (updatedChild, updatedIndex) = rec(e, Nil, first, rest, index + 1)
            if(updatedIndex < 0) (Elem(prefix, label, attributes, scope, true, siblingsBefore ++ (updatedChild :: siblingsAfter):_*), -1)
            else if(updatedIndex == atIndex) (Elem(prefix, label, attributes, scope, true, siblingsBefore ++ (child :: newChild :: siblingsAfter):_*), -1)
            else siblingsAfter match {
              case next :: rest => rec(parent, siblingsBefore :+ child, next, rest, updatedIndex)
              case Nil => (parent, updatedIndex)
            }

          // We have more siblings to sift through
          case (_, _, _, next :: rest) =>
            rec(parent, siblingsBefore :+ child, next, rest, index + 1)

          // We have no children or siblings to check
          case _ => (parent, index + 1)
        }

      (root, root.nonEmptyChildren.filter(isntWhitespace).toList) match {
        case (e:Elem, first :: rest) => rec(e, Nil, first, rest, 1)._1
        case _ => root
      }
    }
  }

}
