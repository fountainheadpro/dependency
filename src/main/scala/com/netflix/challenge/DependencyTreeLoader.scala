package com.netflix.challenge

import scala.io.Source

object DependencyTreeLoader{
  
  //building dependency graph adjacency map
  def apply(input: Source): DependencyTree[String]={
    
    val graphAdjacencyMap=
      input.getLines()
         .map(_.trim.split("->")) //processing file lines
         .filter(_.length==2)     //removing irrelevant lines
         .toVector                //loading data to memory 
         .groupBy(_(0))           //grouping by source node  
         .mapValues(_.map(_(1)).toVector) 
    new DependencyTree[String](graphAdjacencyMap)     
  }
  
} 