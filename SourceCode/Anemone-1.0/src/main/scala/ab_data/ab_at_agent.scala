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
     Abstract Syntax Trees for Agents
     --------------------------------
*/


abstract class AB_AG {
    def ab_toString: String = { " " }
    def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_AG
    def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_AG    
}

case class AB_AST_Empty_Agent() extends AB_AG {
   override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_AG = {
      AB_AST_Empty_Agent() }
   override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_AG = {
      AB_AST_Empty_Agent() }
}

case class AB_AST_Primitive(primitive: AB_Primitive, prim_type: AB_PRIMITIVE_TYPE) extends AB_AG {
    override def ab_toString: String = {
        primitive.ab_toString }
    override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_AST_Primitive = {
        AB_AST_Primitive(primitive.ab_substitute(xs,ts),prim_type) }
    override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_AST_Primitive = {
        AB_AST_Primitive(primitive.ab_simplify(xs,ts),prim_type) }    
}

case class AB_Exec_AST_Primitive(primitive: AB_Primitive, prim_type: AB_PRIMITIVE_TYPE, path:List[Int]) extends AB_AG {
    override def ab_toString: String = {
        primitive.ab_toString }
    override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_AG = {
        AB_Exec_AST_Primitive(primitive.ab_substitute(xs,ts),prim_type,path) }
    override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_AG = {
        AB_Exec_AST_Primitive(primitive.ab_simplify(xs,ts),prim_type,path) }        
}

case class AB_AST_List_Primitive(lag:List[AB_AST_Primitive]) extends AB_AG {
    override def ab_toString: String = {
        "|[ " + (lag.map(x=>x.ab_toString)).mkString("; ") + " ]|" }
    override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_AG = {
        AB_AST_List_Primitive(lag.map(x=>x.ab_substitute(xs,ts))) }
    override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_AG = {
        AB_AST_List_Primitive(lag.map(x=>x.ab_simplify(xs,ts))) }
}

case class AB_Exec_AST_List_Primitive(primitive : AB_AST_Primitive,lag:List[AB_AST_Primitive],path:List[Int]) extends AB_AG {
    override def ab_toString: String = {
        "|[ " + primitive.ab_toString + " ; " + lag.map(x=>x.ab_toString) + " ]|" }
    override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_AG = {
        AB_Exec_AST_List_Primitive(primitive.ab_substitute(xs,ts),lag.map(x=>x.ab_substitute(xs,ts)),path) }
    override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_AG = {
        AB_Exec_AST_List_Primitive(primitive.ab_simplify(xs,ts),lag.map(x=>x.ab_simplify(xs,ts)),path) }
}

case class AB_AST_SEQ_Agent(agi: AB_AG, agii: AB_AG) extends AB_AG {
    override def ab_toString: String = {
        "[ " + agi.ab_toString + " ]" + " ; " + "[ " + agii.ab_toString + " ]" }
    override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_AG = {
        AB_AST_SEQ_Agent(agi.ab_substitute(xs,ts), agii.ab_substitute(xs,ts)) }
    override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_AG = {
        AB_AST_SEQ_Agent(agi.ab_simplify(xs,ts), agii.ab_simplify(xs,ts)) }
}

case class AB_AST_PARA_Agent(agi: AB_AG, agii: AB_AG) extends AB_AG {
    override def ab_toString: String = {
        "[ " + agi.ab_toString + " ]" + " || " + "[ " + agii.ab_toString + " ]" }
    override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_AG = {
        AB_AST_PARA_Agent(agi.ab_substitute(xs,ts), agii.ab_substitute(xs,ts)) }
    override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_AG = {
        AB_AST_PARA_Agent(agi.ab_simplify(xs,ts), agii.ab_simplify(xs,ts)) }
}

case class AB_AST_CHOICE_Agent(agi: AB_AG, agii: AB_AG) extends AB_AG {
    override def ab_toString: String = {
        "[ " + agi.ab_toString + " ]" + " + " + "[ " + agii.ab_toString + " ]" }
    override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_AG = {
        AB_AST_CHOICE_Agent(agi.ab_substitute(xs,ts), agii.ab_substitute(xs,ts)) }
    override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_AG = {
        AB_AST_CHOICE_Agent(agi.ab_simplify(xs,ts), agii.ab_simplify(xs,ts)) }
}

case class AB_AST_GEN_CHOICE_Agent(varsInSets: List[AB_VAR_IN_SET], ag: AB_AG) extends AB_AG {
    override def ab_toString: String = {
        "sum " + varsInSets.map(e=>e.ab_toString).mkString(", ") + " : " + ag.ab_toString }
    override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_AG = {
        AB_AST_GEN_CHOICE_Agent(varsInSets,ag.ab_substitute(xs,ts)) }
    override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_AG = {
        AB_AST_GEN_CHOICE_Agent(varsInSets,ag.ab_simplify(xs,ts)) }
    def ab_listVars:List[String] = { varsInSets.map(x=>x.ab_varN) }
    def ab_list_set_names: List[String] = { varsInSets.map(x=>x.ab_sname) }
}


case class AB_Exec_AST_GEN_CHOICE_Agent(varsInSets: List[AB_VAR_IN_SET], ag: AB_AG, path:List[Int]) extends AB_AG {
    override def ab_toString: String = {
        "sum " + varsInSets.map(e=>e.ab_toString).mkString(", ") + " : " + ag.ab_toString }
    override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_AG = {
        AB_Exec_AST_GEN_CHOICE_Agent(varsInSets,ag.ab_substitute(xs,ts),path) }
    override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_AG = {
        AB_Exec_AST_GEN_CHOICE_Agent(varsInSets,ag.ab_simplify(xs,ts),path) }
    def ab_listVars:List[String] = { varsInSets.map(x=>x.ab_varN) }
    def ab_list_set_names: List[String] = { varsInSets.map(x=>x.ab_sname) }
}


case class AB_IF_THEN_Agent(c: AG_COND, ag : AB_AG) extends AB_AG {
    override def ab_toString: String = {
        "(" + c.ab_toString + ")" + " -> " + "[ " + ag.ab_toString + " ]" }
    override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_AG = {
        AB_IF_THEN_Agent(c.ab_substitute(xs,ts), ag.ab_substitute(xs,ts)) }
    override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_AG = {
        AB_IF_THEN_Agent(c.ab_simplify(xs,ts), ag.ab_simplify(xs,ts)) }
}

case class AB_IF_THEN_ELSE_Agent(c: AG_COND, agi : AB_AG, agii: AB_AG) extends AB_AG {
    override def ab_toString: String = {
        "(" + c.ab_toString + ")" + " -> " + "[ " + agi.ab_toString + " ]" + " <> " + "[ " + agii.ab_toString + " ]" }
    override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_AG = {
        AB_IF_THEN_ELSE_Agent(c.ab_substitute(xs,ts), agi.ab_substitute(xs,ts), agii.ab_substitute(xs,ts)) }
    override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_AG = {
        AB_IF_THEN_ELSE_Agent(c.ab_simplify(xs,ts), agi.ab_simplify(xs,ts), agii.ab_simplify(xs,ts)) }    
}

case class AB_AST_Proc_Call(pn:String,args:List[AB_SI_ELM]) extends AB_AG {
    override def ab_toString: String = {
        if (args == Nil) { pn
	} else { val largs_as_str = args.map(x=>x.ab_toString)
	         pn + "(" + largs_as_str.mkString(",") + ")" } }
    override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_AG = {
        AB_AST_Proc_Call(pn,args.map(x=>x.ab_substitute(xs,ts))) }
    override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_AG = {
        AB_AST_Proc_Call(pn,args.map(x=>x.ab_simplify(xs,ts))) }
}

case class AB_Exec_AST_Proc_Call(pn:String,args:List[AB_SI_ELM],path:List[Int]) extends AB_AG {
    override def ab_toString: String = {
        if (args == Nil) { pn
	} else { val largs_as_str = args.map(x=>x.ab_toString)
	         pn + "(" + largs_as_str.mkString(",") + ")"
	} }
    override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_AG = {
        AB_Exec_AST_Proc_Call(pn,args.map(x=>x.ab_substitute(xs,ts)),path) }
    override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_AG = {
        AB_Exec_AST_Proc_Call(pn,args.map(x=>x.ab_simplify(xs,ts)),path) }
}                             





/*
     Procedure definitions
     ---------------------

     Rem: AB_Abs_Proc, pour AB_Abstract_Procedure class
*/


class AB_Abs_Proc

case class AB_Proc_Def(procedureName:String, procArgs:List[(String,String)], procDef:AB_AG) extends AB_Abs_Proc 
