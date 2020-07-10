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


package ab_data

/*
     Scene definitions
     -----------------
*/


class AB_SCENE_ELMS {
   def ab_toString:String = {" "}
}

case class AB_SCENE_DEFN(name:String, size: AB_SCENE_SIZE, layers: AB_SCENE_LAYER,
          imgs:List[AB_SCENE_IMG_DESC],widgets:List[AB_SCENE_WIDGET_DESC]) extends AB_SCENE_ELMS {
   override def ab_toString:String = { "scene " + name + " of " + size.ab_toString }
}

case class AB_SCENE_SIZE(l:Int,w:Int) extends AB_SCENE_ELMS {
   override def ab_toString: String = { "size = (" + l.toString + "," + w.toString + ")" }
}

case class AB_SCENE_LAYER(list_layers: List[String]) extends AB_SCENE_ELMS {
   override def ab_toString: String = {
       "layers = { " + list_layers.mkString(", ") + " }" }
}

case class AB_SCENE_IMG_DESC(imgName:String,imgFile:String) extends AB_SCENE_ELMS {
   override def ab_toString: String = { imgName + " = " + imgFile }
}

case class AB_SCENE_WIDGET_DESC(wdName:String, lat:List[AB_WD_ATT],
                                display:List[AB_WD_DISPLAY],wdInit:List[AB_WD_ATT_VAL])
extends AB_SCENE_ELMS {
   override def ab_toString: String = { "widget" + wdName } 
}


case class AB_WD_ATT(attName:String,attSet:String) extends AB_SCENE_ELMS {
  override def ab_toString: String = { attName + " in " + attSet }
}

abstract class AB_WD_DISPLAY

case class AB_WD_DISPLAY_SIMPLE(imgName:String) extends AB_WD_DISPLAY {
  def ab_toString: String = { imgName }
}

case class AB_WD_DISPLAY_COND(cond:AG_COND,imgName:String) extends AB_WD_DISPLAY {
  def ab_toString: String = { cond.ab_toString + " -> " + imgName }
}


case class AB_WD_ATT_VAL(attName:String,attValue:String) extends AB_SCENE_ELMS {
   override def ab_toString: String = { attName + " = " + attValue } 
}

