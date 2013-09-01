package com.netflix.challenge

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.io.Source

@RunWith(classOf[JUnitRunner])
class DependencyTreeTests extends FunSuite {
  
      
  test("print out good dependency graph") {
   val depTree=DependencyTreeLoader(Source.fromURL(getClass.getResource("/good_graph.txt")))
   depTree.forEachFrom("A", (x)=> x match {case depTree.TreeNode(node, depth, isLast)=>{
     if (depth>1) print("| ")
     print(" "*(depth-2))
     val splitter=if (isLast) "\\" else "|"
     if (depth>0) print(s"$splitter"+"_ ")
     println(s"$node")
   }})
  }
  
  test("fail on dependency graph with circular dependencies") {
   val depTree=DependencyTreeLoader(Source.fromURL(getClass.getResource("/graph_cd.txt")))
   val thrown=intercept[depTree.CircularDependecyException]{
     depTree.forEachFrom("A",{_=> })
   }
   assert(thrown.getPath.distinct.size<thrown.getPath.size)
  }

  test("fail on empty graphc") {   
   val thrown=intercept[IllegalArgumentException] {
       val depTree=DependencyTreeLoader(Source.fromURL(getClass.getResource("/empty.txt")))
	   depTree.forEachFrom("A", {_=> })
   }
   assert(thrown.getMessage().contains("empty"))
  }
  
  
  
  
  
 
}