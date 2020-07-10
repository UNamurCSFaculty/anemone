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


class ABSceneMemory {


  /* ------------------------------------------------------- */
  /*                                                         */
  /*                    SIZE OF THE SCENE                    */
  /*                                                         */
  /* ------------------------------------------------------- */    

  var abSize = (0,0)

  def abSceneSize(s:AB_SCENE_SIZE) {
    s match {
      case AB_SCENE_SIZE(l,w) => {
      abSize = (l,w) }
    }
  }

  def get_size_scene() = {
    abSize
  }
  

  /* ------------------------------------------------------- */
  /*                                                         */
  /*                   LAYERS OF THE SCENE                   */
  /*                                                         */
  /* ------------------------------------------------------- */    

  var abSceneLayers = ArrayBuffer[String]()

  def layersDefn(l: List[String]) {
    abSceneLayers = ArrayBuffer(l: _*)
  }

  def abDefineLayers(layers: AB_SCENE_LAYER) {
    layers match {
      case AB_SCENE_LAYER(list_layers) => layersDefn(list_layers) }
  }
  
  def findIndexLayerElement(elm: String) = {
    abSceneLayers.indexOf(elm)
  }




  /* ------------------------------------------------------- */
  /*                                                         */
  /*                   IMAGES OF THE SCENE                   */
  /*                                                         */
  /* ------------------------------------------------------- */    

  
  var abBackgroundImg:String=_

  var theImgs = Map[String,(Int,String)]()
  // mapping of the form image_name -> (image number, file name of the image)
  
  var nbImg = 0

  def abSceneBackground(s:String) {
    abBackgroundImg=s
  }

  def get_backgrd_img() = {
    abBackgroundImg
  }
  
  def abAddImg(imgName:String,imgFile:String) {
     if (imgName == "background") {
         abBackgroundImg = imgFile
     } else {
         if (theImgs.contains(imgName)) 
                { theImgs(imgName) = (theImgs(imgName)._1,imgFile) }
         else
                { theImgs = theImgs ++ Map(imgName -> (nbImg,imgFile))
	       nbImg = nbImg+1 }
     }
  }

  def abFindImgIndex(imgName:String):Int = {
      if (theImgs.contains(imgName)) 
             { theImgs(imgName)._1 }
      else
             { -1 }
  }

  def abFindImgFileName(imgName:String):String = {
      theImgs(imgName)._2
  }
  
  def abAddImgsDecl(imgDecls:List[AB_SCENE_IMG_DESC]) {
     for (e <- imgDecls) {
        e match { case AB_SCENE_IMG_DESC(imgName,imgFile) => abAddImg(imgName,imgFile) }
     }
  }
  

  def theImgstoABImgs(): ArrayBuffer[String] = {
     val theImgsSortedByNum=ListMap(theImgs.toSeq.sortBy(_._2):_*)
     var a=ArrayBuffer[String]();
     for ((k,v) <- theImgsSortedByNum) {
        a += v._2
     }
     a
  }
  


  /* ------------------------------------------------------- */
  /*                                                         */
  /*                   WIDGETS OF THE SCENE                  */
  /*                                                         */
  /* ------------------------------------------------------- */    

  // one class instance per widget of the scene

  class abWidgetMem(wn:String,lat:List[AB_WD_ATT],wdisplay:List[AB_WD_DISPLAY],wi:List[AB_WD_ATT_VAL]) {

     // widget name
     val wdName=wn

     // mapping attribute -> value
     var wdAtt=ab_extract_att_names(lat)

     // img definition a pair
     //   - of simple images, being formally an array of image names
     //   - of rules, being formally an array of elements of the form Condition --> image name
     var wdRules=ab_extract_cond_img(wdisplay)

     // initial values as mapping attribute -> value 
     var wdInit=ab_extract_att_init_values(wi)


     def set_layer(wl:String) {
        ab_wd_set_att_value("wdL",AB_SI_ATOMIC(wl))
     }

     def l_sit_att_value:(List[AB_SI_ELM],List[AB_SI_ELM]) = {
        var xs=List[AB_SI_ELM]()
        var ts=List[AB_SI_ELM]()	
        for ( (k,v) <- wdAtt ) {
	   v match {
	     case Some(e) => {
                xs = AB_SI_ATOMIC(k) :: xs
	        ts = e :: ts
             }
             case None => { }
	   }
	}
	(xs,ts)     
     }
        

     def ab_default_att_names = {
       var abMapAttValue=Map[String,Option[AB_SI_ELM]]()
       val abNone:Option[AB_SI_ELM]=None
       abMapAttValue += ("wdX" -> Some(AB_SI_ATOMIC("0")))
       abMapAttValue += ("wdY" -> Some(AB_SI_ATOMIC("0")))
       abMapAttValue += ("wdA" -> Some(AB_SI_ATOMIC("0")))
       abMapAttValue += ("wdV" -> Some(AB_SI_ATOMIC("0")))
       abMapAttValue += ("wdF" -> Some(AB_SI_ATOMIC("100")))
       abMapAttValue += ("wdL" -> abNone)
       abMapAttValue
     }
     
     def ab_extract_att_names(lat:List[AB_WD_ATT]) = {
       var abMapAttValue:Map[String,Option[AB_SI_ELM]] =
               ab_default_att_names
       val abNone:Option[AB_SI_ELM]=None
       for (e <- lat) {
         e match {
	   case AB_WD_ATT(attName,attset) =>
	     { abMapAttValue += (attName -> abNone)
	     }
	 }
       }
       abMapAttValue
     }

     def ab_extract_cond_img(wd:List[AB_WD_DISPLAY]) = {
       var abListImgs=ArrayBuffer[String]()
       var abListCondRules=ArrayBuffer[(AG_COND,String)]()       
       for (e <- wd) {
         e match {
	   case AB_WD_DISPLAY_COND(cond,imgName) => {
	      abListCondRules += ((cond,imgName)) }
	   case AB_WD_DISPLAY_SIMPLE(imgName) => {
	      abListImgs += (imgName) }
	 }
       }
       (abListImgs,abListCondRules)
     }

     def updateWdAttByAttValue(att: String, v: AB_SI_ELM) {
        if (wdAtt.contains(att)) { wdAtt(att) = Some(v) }
     }
     
     def updateWdAttWithInitValues(mpInit: Map[String,AB_SI_ELM]) {
        for ( (k,v) <- mpInit ) { updateWdAttByAttValue(k,v) }
     }
     
     def ab_extract_att_init_values(wi:List[AB_WD_ATT_VAL]) = {
       var abMapAttInitValue = Map[String,AB_SI_ELM]()
       for (e <- wi) {
         e match {
	   case AB_WD_ATT_VAL(attName,attValue) => {
	      abMapAttInitValue += ((attName,AB_SI_ATOMIC(attValue))) }
	 }
       }
       updateWdAttWithInitValues(abMapAttInitValue)
       abMapAttInitValue
     }

     def ab_wd_init_values:(Int,Int,Int,Int,Int,String) = {
        val wdXI = if (wdInit.contains("wdX")) { si_toInt(wdInit("wdX"),0) } else { 0 }
        val wdYI = if (wdInit.contains("wdY")) { si_toInt(wdInit("wdY"),0) } else { 0 }
        val wdAI = if (wdInit.contains("wdA")) { si_toInt(wdInit("wdA"),0) } else { 0 }
        val wdFI = if (wdInit.contains("wdF")) { si_toInt(wdInit("wdF"),100) } else { 100 }
        val wdVI = if (wdInit.contains("wdV")) { si_toInt(wdInit("wdV"),0) } else { 0 }
        val wdLI = if (wdInit.contains("wdL")) { wdInit("wdL").toString } else { "top" }
        (wdXI,wdYI,wdAI,wdFI,wdVI,wdLI)
     }

     def ab_wd_set_att_value(attName:String,si_elm:AB_SI_ELM) {
        if (wdAtt.contains(attName)) {
	  wdAtt(attName) = Some(si_elm)
	} else {
	  wdAtt = wdAtt ++ Map(attName -> Some(si_elm))
        }
     }

     def ab_wd_eval_cond_elm(relop:String,i:Int,j:Int):Boolean = {
        if      ( relop == "=" ) { i==j }
        else if ( relop == "<" ) { i<j  }
        else if ( relop == "<=") { i<=j }
        else if ( relop == ">" ) { i>j  }
        else if ( relop == ">=") { i>=j }
        else { false }
    }
 
    def ab_wd_eval_cond(c: AG_COND, bb:ABBlackboard): Boolean = {
       c match {
          case COND_ATOMIC(relop,argi,argii) => {
             if ( argi.atomicSiElm && argii.atomicSiElm ) {
                val a = argi.functorSiElm
	        val b = argii.functorSiElm
	        val i = bb.ordDecl(a)
	        val j = bb.ordDecl(b)
	        ab_wd_eval_cond_elm(relop,i,j)
             } else {
	        false
	     }
	   }
           case COND_COMPI(relop,cond) => {
	     if (relop == "!") { !ab_wd_eval_cond(cond,bb)
	     } else { false }
	   }
           case COND_COMPII(relop,condi,condii) => {
	      val c = ab_wd_eval_cond(condi,bb)
	      val d = ab_wd_eval_cond(condii,bb)
	      if      ( relop == "&" )  { c && d }
	      else if ( relop == "|" )  { c || d }
	      else { false }
	   }
       }
    }

    var oldImg:Option[String]=None

    def ab_wd_rule_img(bb:ABBlackboard):Option[String] = {
        var eosearch = false
	var img:String=" "
        val l_xs_ts = l_sit_att_value

        val wdListImg = wdRules._1
	val wdListCondImg = wdRules._2

        if (wdListImg.isEmpty) {
             for ( (cond,imgName) <- wdListCondImg ) {
                  if ( (!eosearch) && ab_wd_eval_cond(cond.ab_substitute(l_xs_ts._1,l_xs_ts._2),bb) ) {
                      eosearch = true
	              img = imgName
                  }
             }
        } else {
	          eosearch = true
		  img = wdListImg.head
	}

        if ( (eosearch) && (oldImg!=Some(img)) ) {
	   oldImg=Some(img)
	   Some(img)
	} else {
	   None
	}

     }

  }



  var theWidgetInst=Map[String,(Int,abWidgetMem)]()
  var nbWd = 0


  def abAddWidget(wdName:String,lat:List[AB_WD_ATT], display:List[AB_WD_DISPLAY],wdInit:List[AB_WD_ATT_VAL]) {

     if (theWidgetInst.contains(wdName)) {
       theWidgetInst(wdName) = (theWidgetInst(wdName)._1, new abWidgetMem(wdName,lat,display,wdInit))
     } else {
       theWidgetInst = theWidgetInst ++ Map(wdName -> (nbWd,new abWidgetMem(wdName,lat,display,wdInit)))
       nbWd = nbWd + 1
     }

  }


  def abAddWidgetDecls(widgets:List[AB_SCENE_WIDGET_DESC]) {

     for (w <- widgets) {
        w match {
         case AB_SCENE_WIDGET_DESC(wdName, lat, display, wdInit) => {
	    abAddWidget(wdName, lat, display, wdInit)
	 }
        }
     }

  }


  def abFindWdIndex(wdName:String): Int = {

     if (theWidgetInst.contains(wdName)) {
       theWidgetInst(wdName)._1
     } else {
       -1 
     }

  }

  def abFindWdMemByName(wdName:String): Option[abWidgetMem] = {

     if (theWidgetInst.contains(wdName)) {
       Some(theWidgetInst(wdName)._2)
     } else {
       val abNone:Option[abWidgetMem]=None
       abNone 
     }

  }


  def theWidgetstoABWdNames(): ArrayBuffer[String] = {

     val theWidgetInstSortedByNum=ListMap(theWidgetInst.toSeq.sortBy(_._2._1):_*)
     var a=ArrayBuffer[String]();
     for ((k,v) <- theWidgetInstSortedByNum) {
        a += k
     }
     a

  }



  /* ------------------------------------------------------- */
  /*                                                         */
  /*                  GENERAL DEFINITIONS                    */
  /*                                                         */
  /* ------------------------------------------------------- */    



  def abAddSceneDecls(size:AB_SCENE_SIZE,layers:AB_SCENE_LAYER,
           imgs:List[AB_SCENE_IMG_DESC],widgets:List[AB_SCENE_WIDGET_DESC]) {

     abSceneSize(size)
     abDefineLayers(layers)
     abAddImgsDecl(imgs)
     abAddWidgetDecls(widgets)

  }

  def set_layer(wn:String,l:String) {
     abFindWdMemByName(wn) match {
       case Some(wmem) => { wmem.set_layer(l) }
       case None => { }
     }
  }

  def ab_wd_set_att_value(wn:String,an:String,v:AB_SI_ELM) {
     abFindWdMemByName(wn) match {
       case Some(wmem) => { wmem.ab_wd_set_att_value(an,v) }
       case None => { }
     }
  }

  def ab_wd_init_values(wn:String): (Int,Int,Int,Int,Int,String) = {
     abFindWdMemByName(wn) match {
       case Some(wmem) => { wmem.ab_wd_init_values }
       case None => { (0,0,0,100,0,"top") }
     }
  }

  def ab_wd_rule(wn:String,bb:ABBlackboard):Option[String] = {
     abFindWdMemByName(wn) match {
       case Some(wmem) => { wmem.ab_wd_rule_img(bb) }
       case None => { None }
     }
  }  

}
