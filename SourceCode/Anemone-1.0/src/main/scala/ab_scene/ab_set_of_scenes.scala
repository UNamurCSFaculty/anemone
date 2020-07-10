/***********************************************************************

 Copyright (c) 2019 Jean-Marie Jacquet and the CoordiNam Lab members
 (University of Namur, Belgium)

 Permission is hereby granted, free of charge, to any person obtaining
 a  copy of  this  software and  associated  documentation files  (the
 "Software"), to  deal in the Software  without restriction, including
 without limitation the  rights to use, copy,  modify, merge, publish,
 distribute, sublicense,  and/or sell copies  of the Software,  and to
 permit persons to whom the Software is furnished to do so, subject to
 the following conditions:
 
 The  above  copyright notice  and  this  permission notice  shall  be
 included in all copies or substantial portions of the Software.
 
 THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
 EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
 MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
 NONINFRINGEMENT. IN NO  EVENT SHALL THE AUTHORS  OR COPYRIGHT HOLDERS
 BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY, WHETHER  IN AN
 ACTION OF  CONTRACT, TORT OR  OTHERWISE, ARISING  FROM, OUT OF  OR IN
 CONNECTION WITH  THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
 SOFTWARE.
 
************************************************************************/


package ab_scene

import ab_data._
import ab_blackboard._

import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer


class ABSetOfScenes {

  // association of scene names with scene descriptions
  var currentScenes = Map[String,ABScene]()   

  def addSceneDecl(d:AB_SCENE_DEFN) {

    d match {
        case AB_SCENE_DEFN(name,size,layers,imgs,widgets) => 
           if (! (currentScenes.contains(name)) ) {
	     val newScene = new ABScene(name)
	     newScene.addSceneDecls(size, layers, imgs, widgets)
             currentScenes = currentScenes + (name -> newScene)
	   }
     }

   }


   def addSceneDecls(l: List[AB_SCENE_DEFN]) {

      for (d <- l) { addSceneDecl(d) }

   }


   def place_at(wn:String,s:String,x:Double,y:Double) {
      if (currentScenes.contains(s)) {
        currentScenes(s).place_at(wn,x,y) }
   }

   def move_to(wn:String,s:String,x:Double,y:Double) {
      if (currentScenes.contains(s)) {
        currentScenes(s).move_to(wn,x,y) }
   }
 
   def hide(wn:String,s:String) {
      if (currentScenes.contains(s)) {
        currentScenes(s).hide(wn) }
   }

   def show(wn:String,s:String) {
      if (currentScenes.contains(s)) {
        currentScenes(s).show(wn) }    
  }

  def layer(wn:String,s:String,l:String) {
      if (currentScenes.contains(s)) {
        currentScenes(s).layer(wn,l) }
  }

  def att(an:String,wn:String,s:String,v:AB_SI_ELM,bb:ABBlackboard) {
      println(an + " -- " + wn + "--" + s + "--" + v.ab_toString)
      if (currentScenes.contains(s)) {
        currentScenes(s).att(an,wn,v,bb) }
  }

  def draw_scene(s:String,bb:ABBlackboard) {
      if (currentScenes.contains(s)) {
        currentScenes(s).draw_scene(bb) }
  }

  def redraw() {
     for ( (sn,absc) <- currentScenes ) {
       absc.redraw }
  }
  
}

