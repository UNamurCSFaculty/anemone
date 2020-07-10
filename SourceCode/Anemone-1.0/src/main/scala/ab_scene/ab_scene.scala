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
import scala.collection.immutable.ListMap
import processing.core._
import PConstants._
import PApplet._

class ABScene(nameOfTheScene: String) {

  val theName = nameOfTheScene
  val theMemory = new ABSceneMemory
  val theActor  = new abSceneActor


  def addSceneDecls(size:AB_SCENE_SIZE,layers:AB_SCENE_LAYER, 
           imgs:List[AB_SCENE_IMG_DESC],widgets:List[AB_SCENE_WIDGET_DESC]) {
           theMemory.abAddSceneDecls(size,layers,imgs,widgets)
  }
     

  def place_at(wn:String,x:Double,y:Double) {
        val i=theMemory.abFindWdIndex(wn)
	if (i>=0) { theActor.abObjPlaceAt(i,x,y) }
  }

  def move_to(wn:String,x:Double,y:Double) {
        val i=theMemory.abFindWdIndex(wn)
	if (i>=0) { theActor.abObjMoveTo(i,x,y) }
  }
 
  def hide(wn:String) {
        val i=theMemory.abFindWdIndex(wn)  
	if (i>=0) { theActor.abObjHide(i) }
  }

  def show(wn:String) {
        val i=theMemory.abFindWdIndex(wn)    
	if (i>=0) { theActor.abObjShow(i) }
  }

  def layer(wn:String,l:String) {
        val i=theMemory.abFindWdIndex(wn)
	if (i>=0) {
	   theMemory.set_layer(wn,l)
	   theActor.abObjLayer(i,l)
	}
  }

  def predefinedAtt(an:String) = {
     (an == "wdX") || (an == "wdY") || (an == "wdA") ||
     (an == "wdV") || (an == "wdF") || (an == "wdL")
  }
  
  def att(an:String,wn:String,v:AB_SI_ELM,bb:ABBlackboard) {
         val i=theMemory.abFindWdIndex(wn)
         if (i>=0) {
	   theMemory.ab_wd_set_att_value(wn,an,v)
	   theMemory.ab_wd_rule(wn,bb) match {
	      case Some(img) => {
	          val imgFile = theMemory.abFindImgFileName(img)
                  theActor.abObjNewImg(i,imgFile)
                  }
	      case None =>
	    }
            if (predefinedAtt(an)) { theActor.abObjNewPredefinedAtt(i,an,v) }
	   }
  }

  def propagate_init_values_for_widgets(wdnames: ArrayBuffer[String],bb:ABBlackboard) {
     println(wdnames)
     var i: Int = 0
     var wdDs: (Int,Int,Int,Int,Int,String) = (0,0,0,100,0,"")
     for (wn <- wdnames) {
        println(wn)
        i = theMemory.abFindWdIndex(wn)
	println(i)
	if (i>=0) {
          wdDs = theMemory.ab_wd_init_values(wn)
	  theActor.abObjInitValue(i,wdDs._1,wdDs._2,wdDs._3,wdDs._4,wdDs._5,wdDs._6)
          theMemory.ab_wd_rule(wn,bb) match {
	      case Some(img) => {
	          val imgFile = theMemory.abFindImgFileName(img)
                  theActor.abObjNewImg(i,imgFile)
              }
          }
        }
     }
  }
  
  def draw_scene(bb:ABBlackboard) {
      val s = theMemory.get_size_scene()
      val i = theMemory.get_backgrd_img()
      val a = theMemory.theImgstoABImgs()
      val wn = theMemory.theWidgetstoABWdNames()
      theActor.scene_size(s)
      theActor.scene_background(i)
      theActor.scene_imgs(a)
      theActor.scene_widget_names(wn)
      val title = Array("The AnimBach animation")
      PApplet.runSketch(title, theActor)
      Thread.sleep(2000) 
      propagate_init_values_for_widgets(wn,bb)
  }

  def redraw {
     theActor.redraw
  }

}





