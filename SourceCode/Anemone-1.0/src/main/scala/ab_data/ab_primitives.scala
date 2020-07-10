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
     Abstract Syntax Trees for Primitives
     ------------------------------------
*/

abstract class AB_Primitive {
    def ab_toString: String = { " " }
    def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Primitive
    def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Primitive
}


/*
     Basic primitives
     ~~~~~~~~~~~~~~~~
*/

abstract class AB_Basic_Primitive extends AB_Primitive

case class AB_Tell(stinfo: AB_SI_ELM) extends AB_Basic_Primitive {
    override def ab_toString: String = {
        "tell" + "(" + stinfo.ab_toString + ")" }
    override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Basic_Primitive = {
        AB_Tell(stinfo.ab_substitute(xs,ts)) }
    override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Basic_Primitive = {
        AB_Tell(stinfo.ab_simplify(xs,ts)) }    
}

case class AB_Ask(stinfo: AB_SI_ELM) extends AB_Basic_Primitive {
    override def ab_toString: String = {
        "ask" + "(" + stinfo.ab_toString + ")" }
    override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Basic_Primitive = {
        AB_Ask(stinfo.ab_substitute(xs,ts)) }
    override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Basic_Primitive = {
        AB_Ask(stinfo.ab_simplify(xs,ts)) }    
}

case class AB_Nask(stinfo: AB_SI_ELM) extends AB_Basic_Primitive {
    override def ab_toString: String = {
        "nask" + "(" + stinfo.ab_toString + ")" }
    override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Basic_Primitive = {
        AB_Nask(stinfo.ab_substitute(xs,ts)) }
    override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Basic_Primitive = {
        AB_Nask(stinfo.ab_simplify(xs,ts)) }    
}

case class AB_Get(stinfo: AB_SI_ELM) extends AB_Basic_Primitive {
    override def ab_toString: String = {
        "get" + "(" + stinfo.ab_toString + ")" }
    override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Basic_Primitive = {
        AB_Get(stinfo.ab_substitute(xs,ts)) }
    override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Basic_Primitive = {
        AB_Get(stinfo.ab_simplify(xs,ts)) }    
}


/*
     Multi primitives
     ~~~~~~~~~~~~~~~~
*/

abstract class AB_Multi_Primitive extends AB_Primitive

case class AB_MTell(lstinfo: List[AB_SI_ELM]) extends AB_Multi_Primitive {
    override def ab_toString: String = {
        "mtell" + "(" + lstinfo.map(e=>e.ab_toString) + ")" }
    override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Multi_Primitive = {
        AB_MTell(lstinfo.map(e=>e.ab_substitute(xs,ts))) }
    override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Multi_Primitive = {
        AB_MTell(lstinfo.map(e=>e.ab_simplify(xs,ts))) }    
}

case class AB_MAsk(lstinfo: List[AB_SI_ELM]) extends AB_Multi_Primitive {
    override def ab_toString: String = {
        "mask" + "(" + lstinfo.map(e=>e.ab_toString) + ")" }
    override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Multi_Primitive = {
        AB_MAsk(lstinfo.map(e=>e.ab_substitute(xs,ts))) }
    override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Multi_Primitive = {
        AB_MAsk(lstinfo.map(e=>e.ab_simplify(xs,ts))) }    
}

case class AB_MNask(lstinfo: List[AB_SI_ELM]) extends AB_Multi_Primitive {
    override def ab_toString: String = {
        "mnask" + "(" + lstinfo.map(e=>e.ab_toString) + ")" }
    override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Multi_Primitive = {
        AB_MNask(lstinfo.map(e=>e.ab_substitute(xs,ts))) }
    override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Multi_Primitive = {
        AB_MNask(lstinfo.map(e=>e.ab_simplify(xs,ts))) }    
}

case class AB_MGet(lstinfo: List[AB_SI_ELM]) extends AB_Multi_Primitive {
    override def ab_toString: String = {
        "mget" + "(" + lstinfo.map(e=>e.ab_toString) + ")" }
    override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Multi_Primitive = {
        AB_MGet(lstinfo.map(e=>e.ab_substitute(xs,ts))) }
    override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Multi_Primitive = {
        AB_MGet(lstinfo.map(e=>e.ab_simplify(xs,ts))) }    
}


/*
     Primitives for processes
     ~~~~~~~~~~~~~~~~~~~~~~~~
*/

abstract class AB_Proc_Primitive extends AB_Primitive

case class AB_PTell(pn:String,args:List[AB_SI_ELM]) extends AB_Proc_Primitive {
    override def ab_toString: String = {
           if (args == Nil) { "tellp" + "(" + pn +")"
	   } else { val largs_as_str = args.map(x=>x.ab_toString)
	            "tellp" + "(" + pn + "(" + largs_as_str.mkString(",") + "))" 
           } }
    override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Proc_Primitive = {
        AB_PTell(pn,args.map(x=>x.ab_substitute(xs,ts))) }
    override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Proc_Primitive = {
        AB_PTell(pn,args.map(x=>x.ab_simplify(xs,ts))) }    
}

case class AB_PAsk(pn:String,args:List[AB_SI_ELM]) extends AB_Proc_Primitive {
    override def ab_toString: String = {
           if (args == Nil) { "askp" + "(" + pn +")"
	   } else { val largs_as_str = args.map(x=>x.ab_toString)
	            "askp" + "(" + pn + "(" + largs_as_str.mkString(",") + "))" 
           } }
    override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Proc_Primitive = {
        AB_PAsk(pn,args.map(x=>x.ab_substitute(xs,ts))) }
    override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Proc_Primitive = {
        AB_PAsk(pn,args.map(x=>x.ab_simplify(xs,ts))) }    
}

case class AB_PNask(pn:String,args:List[AB_SI_ELM]) extends AB_Proc_Primitive {
    override def ab_toString: String = {
           if (args == Nil) { "naskp" + "(" + pn +")"
	   } else { val largs_as_str = args.map(x=>x.ab_toString)
	            "naskp" + "(" + pn + "(" + largs_as_str.mkString(",") + "))" 
           } }
    override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Proc_Primitive = {
        AB_PNask(pn,args.map(x=>x.ab_substitute(xs,ts))) }
    override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Proc_Primitive = {
        AB_PNask(pn,args.map(x=>x.ab_simplify(xs,ts))) }    
}

case class AB_PGet(pn:String,args:List[AB_SI_ELM]) extends AB_Proc_Primitive {
    override def ab_toString: String = {
           if (args == Nil) { "getp" + "(" + pn +")"
	   } else { val largs_as_str = args.map(x=>x.ab_toString)
	            "getp" + "(" + pn + "(" + largs_as_str.mkString(",") + "))" 
           } }
    override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Proc_Primitive = {
        AB_PGet(pn,args.map(x=>x.ab_substitute(xs,ts))) }
    override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Proc_Primitive = {
        AB_PGet(pn,args.map(x=>x.ab_simplify(xs,ts))) }    
}



/*
     Primitives for rules
     ~~~~~~~~~~~~~~~~~~~~
*/

abstract class AB_Rule_Primitive extends AB_Primitive

case class AB_RTell(rn: String) extends AB_Rule_Primitive {
    override def ab_toString: String = {
           "tellr(" + rn + ")"
    }
    override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Rule_Primitive = {
        AB_RTell( rn ) }
    override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Rule_Primitive = {
        AB_RTell( rn ) }    
}

case class AB_RAsk(rn: String) extends AB_Rule_Primitive {
    override def ab_toString: String = {
           "askr(" + rn + ")"
    }
    override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Rule_Primitive = {
        AB_RAsk( rn) }
    override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Rule_Primitive = {
        AB_RAsk( rn ) }
}

case class AB_RNask(rn: String) extends AB_Rule_Primitive {
    override def ab_toString: String = {
           "naskr(" + rn + ")"
    }
    override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Rule_Primitive = {
        AB_RNask( rn ) }
    override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Rule_Primitive = {
        AB_RNask( rn ) } 
}

case class AB_RGet(rn: String) extends AB_Rule_Primitive {
    override def ab_toString: String = {
           "getr(" + rn + ")"
    }
    override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Rule_Primitive = {
        AB_RGet( rn ) }
    override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Rule_Primitive = {
        AB_RGet( rn ) } 
}



/*
     Primitives for scene
     ~~~~~~~~~~~~~~~~~~~~
*/

abstract class AB_Scene_Primitive extends AB_Primitive

case class AB_Draw_Scene(scene:String) extends AB_Scene_Primitive {
    override def ab_toString: String = { "draw(" + scene + ")" }
    override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Scene_Primitive = {
         AB_Draw_Scene(scene) }
    override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Scene_Primitive = {
         AB_Draw_Scene(scene) }
}

case class AB_Place_Widget(w:String,s:String,x:AB_SI_ELM,y:AB_SI_ELM) extends AB_Scene_Primitive {
    override def ab_toString: String = {
         "place_at(" + w + "," + s + "," + x.ab_toString + "," + y.ab_toString + ")" }
    override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Scene_Primitive = {
         AB_Place_Widget(w,s,x.ab_substitute(xs,ts),y.ab_substitute(xs,ts)) }
    override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Scene_Primitive = {
         AB_Place_Widget(w,s,x.ab_simplify(xs,ts),y.ab_simplify(xs,ts)) }
}

case class AB_Widget_Moveto(w:String,s:String,x:AB_SI_ELM,y:AB_SI_ELM) extends AB_Scene_Primitive {
    override def ab_toString: String = {
         "move_to(" + w + "," + s + "," + x.ab_toString + "," + y.ab_toString + ")" }
    override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Scene_Primitive = {
         AB_Widget_Moveto(w,s,x.ab_substitute(xs,ts),y.ab_substitute(xs,ts)) }
    override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Scene_Primitive = {
         AB_Widget_Moveto(w,s,x.ab_simplify(xs,ts),y.ab_simplify(xs,ts)) }
}

case class AB_Hide_Widget(w:String,s:String) extends AB_Scene_Primitive {
    override def ab_toString: String = {
         "hide(" + w + "," + s + ")" }
    override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Scene_Primitive = {
         AB_Hide_Widget(w,s) }
    override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Scene_Primitive = {
         AB_Hide_Widget(w,s) }
}

case class AB_Show_Widget(w:String,s:String) extends AB_Scene_Primitive {
    override def ab_toString: String = {
         "show(" + w + "," + s + ")" }
    override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Scene_Primitive = {
         AB_Show_Widget(w,s) }
    override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Scene_Primitive = {
         AB_Show_Widget(w,s) }
}

case class AB_Layer_Widget(w:String,s:String,l:String) extends AB_Scene_Primitive {
    override def ab_toString: String = {
         "layer(" + w + "," + s + "," + l + ")" }
    override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Scene_Primitive = {
         AB_Layer_Widget(w,s,l) }
    override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Scene_Primitive = {
         AB_Layer_Widget(w,s,l) }
}

case class AB_Att_Widget(x:AB_SI_ELM,w:String,s:String,v:AB_SI_ELM) extends AB_Scene_Primitive {
    override def ab_toString: String = {
         "att(" + x.ab_toString + w + "," + s + "," + v.ab_toString + ")" }
    override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Scene_Primitive = {
         AB_Att_Widget(x.ab_substitute(xs,ts),w,s,v.ab_substitute(xs,ts)) }
    override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_Scene_Primitive = {
         AB_Att_Widget(x.ab_simplify(xs,ts),w,s,v.ab_simplify(xs,ts)) }
}



/*
     Primitive Types
     ~~~~~~~~~~~~~~~
*/

abstract class AB_PRIMITIVE_TYPE {
    def ab_toString: String = { " " }
}

case class AB_Prim_Type_Basic() extends AB_PRIMITIVE_TYPE {
    override def ab_toString: String = { "basic" }
}

case class AB_Prim_Type_Multi() extends AB_PRIMITIVE_TYPE {
    override def ab_toString: String = { "multi" }
}

case class AB_Prim_Type_Proc() extends AB_PRIMITIVE_TYPE {
    override def ab_toString: String = { "proc" }
}

case class AB_Prim_Type_Rule() extends AB_PRIMITIVE_TYPE {
    override def ab_toString: String = { "rule" }
}

case class AB_Prim_Type_Scene() extends AB_PRIMITIVE_TYPE {
    override def ab_toString: String = { "scene" }
}















