package com.netflix.challenge

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import scala.math.Ordering
import scala.collection.mutable.Stack

class DependencyTree[Node](graph: Map[Node, Vector[Node]]){
    
  case class TreeNode(el: Node, depth: Int, isLast: Boolean){
        
    override def toString=el.toString
    
  }
  
  implicit val OrderingTreeNode = Ordering.by[TreeNode, Int]{_.depth} 
 
 
  class CircularDependecyException(msg: String, path: Vector[Node]) extends RuntimeException(msg){
    def getPath=this.path
  }


  def forEachFrom(startNode: Node, f: TreeNode=>Unit):Unit={
    
    require(graph.size>0,"Unable to work with empty dependency tree")  
    
    @tailrec
    def dftPath(currPath: Vector[Node], toVisit: Vector[TreeNode]): Unit={
            
      toVisit match {
        case currentNode+:tail=>{
          f(currentNode)
          val nextBlock=graph.getOrElse(currentNode.el, Vector())
          val nextBlockTreeNodes=nextBlock.map(el=>TreeNode(el, currPath.size+1, el==nextBlock.last))
          val toVisitNext = nextBlockTreeNodes++:tail     
          val nextPath: Vector[Node]=nextBlockTreeNodes.headOption match{
            //going deeper
            case Some(nextNode)=>
              //circular dependency check before adding an element to the path
              if (!currPath.contains(currentNode.el)) 
                currentNode.el+:currPath 
              else 
                throw new CircularDependecyException("Circular dependecy detected", currPath.reverse:+currentNode.el)
            //retracting the path to the last parent
            case None => tail match {
              case Vector()=>Vector[Node]()
              case head+:tail=>currPath.takeRight(head.depth)
            }
          }
          dftPath(nextPath, toVisitNext)
        }
        case _ => Unit
      }
    }    
    
    dftPath(Vector[Node](), Vector(TreeNode(startNode, 0, false)))

  }
  
}
 
