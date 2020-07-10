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

import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.ListMap
import processing.core._
import PConstants._
import PApplet._



class abSceneActor extends PApplet {

  /* ------------------------------------------------------------------------- */
  /*                                                                           */
  /*                     DECLARATION OF VARIABLES                              */
  /*                                                                           */
  /* ------------------------------------------------------------------------- */  

  var abLength = 1000
  var abWidth = 1000

  var currentPImg:PImage=_
  var currentAnimObj:abAnimObj=_

  var theWidgetNames=ArrayBuffer[String]()
  var theSceneObjs=ArrayBuffer[abAnimObj]()

  var bkgrdImg:String = _
  var theRealImgs=ArrayBuffer[String]()
  var theVarImgs=ArrayBuffer[PImage]()
  var abBackgroundImg:PImage = _

  var theLayers=ArrayBuffer[String]()


  /* ------------------------------------------------------------------------- */
  /*                                                                           */
  /*                      INNER CLASS abAnimObj                                */
  /*                                                                           */
  /*   Depicts animated objects at the Processing level. Basically, such       */
  /*   objects are characterized by x, y coordinates, the layer, and an        */
  /*   images. All these features may be changed by functions defined below.   */
  /*                                                                           */
  /*   Note that attribute handling and images to be selected by display       */
  /*   rules are achieved at the scene level.                                  */
  /*                                                                           */
  /* ------------------------------------------------------------------------- */  

  class abAnimObj(cx:Double,cy:Double,ca:Int,cf:Int,cv:Boolean,layer:String) {
    
     var x=cx
     var y=cy
     var dx=0.0
     var dy=0.0
     var tx=0.0
     var ty=0.0
     var epsilon=0.01

     var a=ca  // angle in degrees
     var sf=cf // scale factor in percentage (ex: 50 to indicate half size)
     
     var toBeDisplayed = cv
     var img:PImage=_

     var theLayer=layer
     
     var abNbFrame = 200

     def abImgAnimObj(i:PImage) {
       img = i
     }
     
     def abPlaceAt(nx:Double,ny:Double) {
       x=nx; y=ny; tx=nx; ty=ny
     }
     
     def abMoveTo(nx:Double,ny:Double) {
       tx = nx; ty = ny
       dx = (nx-x)/abNbFrame
       dy = (ny-y)/abNbFrame
     }
     
     def abMaj() {
       if (abs((tx-x).toFloat) <= epsilon) { x = tx } else { x = x+dx }
       if (abs((ty-y).toFloat) <= epsilon) { y = ty } else { y = y+dy }     
     }
     
     def abDisplay() {
        if (toBeDisplayed) {
	   pushMatrix()
	   translate(x.toFloat,y.toFloat)
	   rotate(radians(a))
	   scale(sf.toFloat/100)
	   image(img,0,0)
	   popMatrix()
	}
     }
  
     def abHide() {
       toBeDisplayed = false
     }
  
     def abShow() {
       toBeDisplayed = true
     }

     def abLayer(l:String) {
       theLayer = l
     }

     def abInitValues(wdXI:Int,wdYI:Int,wdAI:Int,wdFI:Int,wdVI:Int,wdLI:String) {
       x = wdXI; y = wdYI; a = wdAI; sf = wdFI;
       theLayer = wdLI
       toBeDisplayed = (wdVI == 1)
     }


     def abNewImg(imgName:String) {
        if (theRealImgs.contains(imgName)) {
	  val i=theRealImgs.indexOf(imgName)
	  img=theVarImgs(i)
	}
     }

     def abNewPredefinedAtt(attName:String,v:AB_SI_ELM) {
        attName match {
           case "wdX" => {
	      val v_as_double = si_toDouble(v,-1.0)
	      if ( v_as_double != -1.0 ) {
                 val nx = v_as_double
		 println("nx : " + nx + " x " + x)
		 tx = nx
	         dx = (nx-x)/abNbFrame }
	   }
           case "wdY" => {
	      val v_as_double = si_toDouble(v,-1.0)
	      if ( v_as_double != -1.0 ) { 
	         val ny = v_as_double
		 ty = ny
	         dy = (ny-y)/abNbFrame }
	   }
           case "wdA" => { a = si_toInt(v,-1) }
	   case "wdF" => { sf = si_toInt(v,-1) }
	   case "wdV" => {
	      val v_as_int = si_toInt(v,0)
	      toBeDisplayed = (v_as_int == 1) 
	   }
	   case "wdL" => {
	      val v_as_string = v.ab_toString
	      theLayer = v_as_string
	   }
	   case _ => {
	      println("Error in the specification of predefined attribute")
	   }
        }
     }

  }


  /* ------------------------------------------------------------------------- */
  /*                                                                           */
  /*                      METHODS FOR THE SCENE                                */
  /*                                                                           */
  /* ------------------------------------------------------------------------- */  


  override def settings() {
    size(abLength,abWidth)
  }
  
  override def setup() {

    abBackgroundImg=loadImage(bkgrdImg)
    background(abBackgroundImg)

    for (i <- 0 until theRealImgs.length) {
      currentPImg = loadImage(theRealImgs(i))
      theVarImgs += currentPImg
    }

    for (i <- 0 until theWidgetNames.length) {
      currentAnimObj = new abAnimObj(0,0,0,100,false,"top")
      theSceneObjs += currentAnimObj
    }

    println("end setup")
    println("The scene objs length "+ theSceneObjs.length)
  }
  
  override def draw() {
     background(abBackgroundImg)
     for (i <- 0 until theWidgetNames.length) {     
        theSceneObjs(i).abMaj()
        theSceneObjs(i).abDisplay()
     }
  }

  def scene_size(ss:(Int,Int)) {
    abLength = ss._1; abWidth = ss._2
  }

  def scene_background(s: String) {
    bkgrdImg = s
  }

  def scene_imgs(a:ArrayBuffer[String]) {
    theRealImgs = a
  }

  def scene_widget_names(wn:ArrayBuffer[String]) {
    theWidgetNames = wn
  }

  def abObjPlaceAt(i:Int,nx:Double,ny:Double) {
    theSceneObjs(i).abPlaceAt(nx,ny)
  }

  def abObjMoveTo(i:Int,nx:Double,ny:Double) {
    theSceneObjs(i).abMoveTo(nx,ny)
  }
    
  def abObjMaj(i:Int) {
    theSceneObjs(i).abMaj()
  }
    
  def abObjDisplay(i:Int) {
    theSceneObjs(i).abDisplay()
  }
    
  def abObjHide(i:Int) {
    theSceneObjs(i).abHide()
  }
    
  def abObjShow(i:Int) {
    theSceneObjs(i).abShow()
  }

  def abObjLayer(i:Int,l:String) {
    theSceneObjs(i).abLayer(l) 
  }

  def abObjInitValue(i:Int,wdXI:Int,wdYI:Int,wdAI:Int,wdFI:Int,wdVI:Int,wdLI:String) {
    println(theSceneObjs.length)
    theSceneObjs(i).abInitValues(wdXI,wdYI,wdAI,wdFI,wdVI,wdLI)
  }

  def abObjNewImg(i:Int,img:String) {
    theSceneObjs(i).abNewImg(img)
  }

  def abObjNewPredefinedAtt(i:Int,attName:String,v:AB_SI_ELM) {
    println("abObjPre -- " + attName + "--" + v.ab_toString)
    theSceneObjs(i).abNewPredefinedAtt(attName,v)
  }

}
