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


package ab_parser

import ab_data._

import scala.util.parsing.combinator._
import scala.util.matching.Regex

class AnimBachParsers extends RegexParsers {

  def idLC      : Parser[String] = ("[a-z][0-9a-zA-Z_]*").r ^^ {_.toString}
  def idUC      : Parser[String] = ("[A-Z][0-9a-zA-Z_]*").r ^^ {_.toString}
  def idAC      : Parser[String] = ("[0-9a-zA-Z_]+").r ^^ {_.toString}    
  def idList    : Parser[List[String]] = rep1sep(idLC, ",")
  def idACList  : Parser[List[String]] = rep1sep(idAC, ",")
  def idInt     : Parser[Int] = ("[0-9]+").r ^^ {_.toInt}
  def idPathFile: Parser[String] = ("[.0-9a-zA-Z_/\"]+").r ^^ {_.toString}

  /*       Main concepts                   */

  def aboptset    : Parser[Any] = opt(setSpec) ^^ {
         case None => None
         case Some(ab) => ab }
  def aboptmap    : Parser[Any] = opt(mapSpec) ^^ { 
         case None => None
         case Some(ab) => ab }
  def abopteqn    : Parser[Any] = opt(eqnSpec) ^^ { 
         case None => None
         case Some(ab) => ab }
  def aboptscene  : Parser[Any] = opt(sceneSpec) ^^ { 
         case None => None
         case Some(ab) => ab }
  def aboptproc   : Parser[Any] = opt(procDecls) ^^ { 
         case None => None
         case Some(ab) => ab }
  def aboptrule   : Parser[Any] = opt(ruleDecls) ^^ { 
         case None => None
         case Some(ab) => ab }

  def abprgm   : Parser[(Any, Any, Any, Any, Any, Any)] = 
                       aboptset ~ aboptmap ~ abopteqn ~ aboptscene ~ aboptproc ~ aboptrule ^^ {
		       case abset ~ abmap ~ abeqn ~ abscene ~ abproc ~ abrule =>
		              (abset, abmap, abeqn, abscene, abproc, abrule) }


  /*      rule description         */

  def ruleDecls : Parser[List[AB_GEN_RULE]] =
    "rule" ~ rep1(ruleDef) ^^ {
        case _ ~ lruleDef => lruleDef }

  def ruleDef : Parser[AB_GEN_RULE] =
    ruleName ~ "= " ~ optVarsInFor ~ preCRule ~ "-->" ~ postCRule ~ "." ^^ {
        case rn ~ _ ~ lVarsInFor ~ pre ~ _ ~ post ~ _ =>
	  AB_RULE(rn,lVarsInFor,pre,post) }

  def ruleName = idLC

  def optVarsInFor : Parser[List[AB_VAR_IN_SET]] = opt(lVarsInFor) ^^ {
     case None => List[AB_VAR_IN_SET]()
     case Some(lv) => lv }

  def lVarsInFor : Parser[List[AB_VAR_IN_SET]] =
     "for" ~> rep1sep(varInSet,",") <~ ":"

  def varInSet : Parser[AB_VAR_IN_SET] = idLC ~ "in" ~ idUC ^^ {
     case vn ~ _ ~ sn => AB_VAR_IN_SET(vn,sn) }

  def preCRule : Parser[List[AB_SIGNED_SI_PN_ELM]] = lSiStPnElm
  def postCRule : Parser[List[AB_SIGNED_SI_PN_ELM]] = lSiStPnElm




  /*       scene description        */


  def sceneSpec : Parser[List[AB_SCENE_DEFN]] =
    "scene"~ rep1(sceneDef) ^^ {
        case _ ~ lSceneDef => lSceneDef }

  def sceneDef : Parser[AB_SCENE_DEFN] =
       idLC ~ "=" ~ "{" ~ sizeScene~layersScene~imgsScene~listWidgetDesc ~ "}" ~ "." ^^ {
         case name ~ _ ~ _ ~ size ~ layers ~ imgs ~ widgets ~ _ ~ _ => 
            AB_SCENE_DEFN(name, size, layers, imgs, widgets) }

  def sizeScene : Parser[AB_SCENE_SIZE] = "size" ~ "=" ~ "("~idInt~","~idInt~")" ~ "." ^^ {
        case _ ~ _ ~ _ ~ lg ~ _ ~ wd ~ _ ~ _ => AB_SCENE_SIZE(lg,wd) }

  def layersScene : Parser[AB_SCENE_LAYER] = "layers" ~ "=" ~ "{" ~ rep1sep(layerElm,",") ~ "}" ~ "." ^^ {
        case _ ~ _ ~ _ ~ lelm ~ _ ~ _ => AB_SCENE_LAYER(lelm) }

  def layerElm = idLC

  def imgDef    : Parser[AB_SCENE_IMG_DESC] = idLC~"="~"loadImage("~idPathFile~")" ~ "." ^^ {
        case imgName ~ _ ~ _ ~ imgFile ~ _ ~ _ => AB_SCENE_IMG_DESC(imgName,imgFile) }
	
  def imgsScene : Parser[List[AB_SCENE_IMG_DESC]] = rep1(imgDef)

  def listWidgetDesc : Parser[List[AB_SCENE_WIDGET_DESC]] = rep(widgetDesc)
  
  def widgetDesc : Parser[AB_SCENE_WIDGET_DESC] =
    "widget" ~ widgetName ~ "=" ~ "{" ~ wdAttributes ~ wdDisplays ~ wdInits ~ "}" ^^ {
       case _ ~ wdN ~ _ ~ _ ~ wdlAtt ~ wdlDisplay ~ wdLay ~ _ => 
           AB_SCENE_WIDGET_DESC(wdN,wdlAtt,wdlDisplay,wdLay) }

  def widgetName : Parser[String] = idLC

  def wdAttributes : Parser[List[AB_WD_ATT]] = opt(realWdAttributes) ^^ {
       case None => List[AB_WD_ATT]()
       case Some(l_ab_wd_att) => l_ab_wd_att
  }
  
  def realWdAttributes : Parser[List[AB_WD_ATT]] = "attributes" ~ "=" ~ "{" ~ rep(wdAtt) ~ "}" ^^ {
       case _ ~ _ ~ _ ~ latt ~ _  => latt }

  def wdAtt : Parser[AB_WD_ATT] = idLC ~ "in" ~ idUC ~ "."  ^^ {
      case attName ~ _ ~ attSet ~ _  => AB_WD_ATT(attName,attSet) }

  def wdDisplays : Parser[List[AB_WD_DISPLAY]] =
       "display" ~ "=" ~ "{" ~ rep(wdDisplay) ~ "}" ^^ {
        case   _ ~ _ ~ _ ~ ldisplay ~ _  => ldisplay }
  
  def wdDisplay : Parser[AB_WD_DISPLAY] = wdDisplayC | wdDisplayS

  def wdDisplayS : Parser[AB_WD_DISPLAY] = idLC ^^ {  
     case imgName  => AB_WD_DISPLAY_SIMPLE(imgName) }

  def wdDisplayC : Parser[AB_WD_DISPLAY] = abCond ~ "->" ~ idLC ~ "." ^^ {  
     case cond ~ _ ~ imgName ~ _ => AB_WD_DISPLAY_COND(cond,imgName) }

  def wdInits : Parser[List[AB_WD_ATT_VAL]] = opt(realWdInits) ^^ {
     case None => List[AB_WD_ATT_VAL]()
     case Some(l_ab_wd_att_val) => l_ab_wd_att_val
  }
     
  def realWdInits : Parser[List[AB_WD_ATT_VAL]] = "init" ~ "=" ~ "{" ~ rep(wdInit) ~ "}" ^^ {
     case _ ~ _ ~ _ ~ linits ~ _ => linits }
    
  def wdInit : Parser[AB_WD_ATT_VAL] = idLC ~ "=" ~ idLC ~ "." ^^ {
     case attName ~ _ ~ attValue ~ _ =>  AB_WD_ATT_VAL(attName,attValue) }


  /*       set specifications       */

  def setKeyword : Parser[String] = "set" | "eset"
  
  def setSpec   : Parser[List[AB_SET]] = setKeyword ~ setDecls ^^ {
        case _ ~ sdecls => sdecls }

  def setDecl   : Parser[AB_SET] = idUC~"="~"{"~idACList~"}"~"." ^^ {
        case setdfned ~ _ ~ _ ~ elmIds ~ _ ~ _ => AB_SET_DEF(setdfned,elmIds) }
	
  def setDecls  : Parser[List[AB_SET]] = rep1(setDecl)


  /*       map specifications    */


  def mapSpec   : Parser[String] = "map" ^^ { case s => s }
  

  def eqnSpec   : Parser[List[AB_MAP_EQ]] = "eqn"~eqnDecls ^^ {
         case _ ~ mdecls => mdecls }

  def eqnDecl     : Parser[AB_MAP_EQ] = stInfo ~ "=" ~ stInfo ~ "." ^^ {
         case sti ~ _ ~ stii ~ _ => AB_MAP_EQ(sti,stii) }
  
  def eqnDecls  : Parser[List[AB_MAP_EQ]] = rep1(eqnDecl)


  /*          Structured information            */

  def stInfoArgs : Parser[List[AB_SI_ELM]] = "("~rep1sep(stInfo, ",")~")" ^^ {
         case _ ~ stl ~ _ => stl }
	 
  def stInfo     : Parser[AB_SI_ELM] = idAC~opt(stInfoArgs) ^^ {
         case f ~ None =>  AB_SI_ATOMIC(f) 
	 case f ~ Some(stl) => AB_SI_COMPOSED(f,stl)  }

  def stLCInfo     : Parser[AB_SI_ELM] = idLC~opt(stInfoArgs) ^^ {
         case f ~ None =>  AB_SI_ATOMIC(f) 
	 case f ~ Some(stl) => AB_SI_COMPOSED(f,stl)  }

  def signSIElm : Parser[AB_SIGN] =
      "+" ^^ { case s => ab_sign_plus() } |
      "-" ^^ { case s => ab_sign_minus() } 

  def signedStPnInfo : Parser[AB_SIGNED_SI_PN_ELM] = signSIElm ~ stPnInfo ^^ {
         case s ~ AB_AST_Proc_Call(pn,largs) => AB_SPC_ELM(s,pn,largs)
         case s ~ AB_SI_ATOMIC(fn) => AB_SSI_ELM(s,AB_SI_ATOMIC(fn))
         case s ~ AB_SI_COMPOSED(fn,largs) => AB_SSI_ELM(s,AB_SI_COMPOSED(fn,largs)) }

  def stPnInfo = stLCInfo | procedureCall
  
  def lSiStPnElm : Parser[List[AB_SIGNED_SI_PN_ELM]] = repsep(signedStPnInfo, ",") ^^ {
         case lSiElm => lSiElm }


  /*    Structured information with variables     */

  def stLVarInfo : Parser[List[AB_VAR_SI_ELM]] = rep1sep(stVarInfo, ",") ^^ {
         case lf => lf }

  def stVarInfoArgs : Parser[List[AB_VAR_SI_ELM]] = "("~rep1sep(stVarInfo, ",")~")" ^^ {
         case _ ~ stl ~ _ => stl }
	 
  def stVarInfo : Parser[AB_VAR_SI_ELM] =  stVar | stVarCompInfo

  def stVar : Parser[AB_VAR_SI_ELM] = "?" ~ idUC ^^ {
         case _ ~ varName => AB_VAR_ELM(varName) }

  def stSimpleSI : Parser[AB_VAR_SI_ELM] = idAC ^^ {
         case si => AB_VAR_SI_TERM(AB_SI_ATOMIC(si)) }

  def stVarCompInfo     : Parser[AB_VAR_SI_ELM] = idAC~opt(stVarInfoArgs) ^^ {
         case f ~ None =>  AB_VAR_SI_TERM(AB_SI_ATOMIC(f))
	 case f ~ Some(stl) => AB_VAR_SI_COMPOSED(f,stl)  }
  


  /*          Conditions                        */

  val op_equal : Parser[String] = "="
  val op_sless : Parser[String] = "<"
  val op_leq   : Parser[String] = "<="
  val op_sge   : Parser[String] = ">"
  val op_geq   : Parser[String] = ">="
  def relop    : Parser[String] = op_equal | op_leq | op_sless | op_geq | op_sge

  val op_or    : Parser[String] = "|"
  val op_and   : Parser[String] = "&"
  val op_neg   : Parser[String] = "!"

  def primitiveCond  : Parser[AG_COND] = stInfo ~ relop ~ stInfo ^^ {
         case si ~ r ~ sii => COND_ATOMIC(r,si,sii) }

  def abCond = orCond

  def orCond   : Parser[AG_COND] = andCond ~ rep(op_or~orCond) ^^ {
       case c ~ List() => c
       case ci ~List(op~cii) => COND_COMPII(op,ci,cii) }

  def andCond  : Parser[AG_COND] = simpleCond ~ rep(op_and~andCond) ^^ {
       case c ~ List() => c
       case ci ~List(op~cii) => COND_COMPII(op,ci,cii) }

  def simpleCond : Parser[AG_COND] = primitiveCond | negCond | parenthesizedCond

  def negCond  : Parser[AG_COND] = op_neg~abCond ^^ {
       case op~c => COND_COMPI(op,c) }
       
  def parenthesizedCond : Parser[AG_COND] = "("~>abCond<~")"


/*          AnimBach primitives                   */

  def basic_primitive : Parser[AB_Primitive] =
     "tell("~stInfo~")" ^^ {
            case _ ~ sti ~ _  => AB_Tell(sti) }  | 
     "ask("~stInfo~")" ^^ {
            case _ ~ sti ~ _  => AB_Ask(sti) }   | 
     "get("~stInfo~")" ^^ {
            case _ ~ sti ~ _  => AB_Get(sti) }   | 
     "nask("~stInfo~")" ^^ {
            case _ ~ sti ~ _  => AB_Nask(sti) }

  def multi_primitive : Parser[AB_Primitive] =
     "mtell(" ~ repsep(stInfo, ",") ~ ")" ^^ {
            case _ ~ lsti ~ _  => AB_MTell(lsti) }  | 
     "mask(" ~ repsep(stInfo, ",") ~ ")" ^^ {
            case _ ~ lsti ~ _  => AB_MAsk(lsti) }   | 
     "get(" ~ repsep(stInfo, ",") ~ ")" ^^ {
            case _ ~ lsti ~ _  => AB_MGet(lsti) }   | 
     "nask(" ~ repsep(stInfo, ",") ~ ")" ^^ {
            case _ ~ lsti ~ _  => AB_MNask(lsti) }

  def proc_primitive : Parser[AB_Primitive] =
     "tellp("~ idUC~opt(stInfoArgs) ~ ")" ^^ {
            case _ ~ pn ~ None ~ _  => AB_PTell(pn,List[AB_SI_ELM]())
            case _ ~ pn ~ Some(largs) ~ _  => AB_PTell(pn,largs) }  | 
     "askp("~ idUC~opt(stInfoArgs) ~ ")" ^^ {
            case _ ~ pn ~ None ~ _  => AB_PAsk(pn,List[AB_SI_ELM]())
            case _ ~ pn ~ Some(largs) ~ _  => AB_PAsk(pn,largs) }  | 
     "getp("~ idUC~opt(stInfoArgs) ~ ")" ^^ {
            case _ ~ pn ~ None ~ _  => AB_PGet(pn,List[AB_SI_ELM]())
            case _ ~ pn ~ Some(largs) ~ _  => AB_PGet(pn,largs) }  | 
     "naskp("~ idUC~opt(stInfoArgs) ~ ")" ^^ {
            case _ ~ pn ~ None ~ _  => AB_PNask(pn,List[AB_SI_ELM]())
            case _ ~ pn ~ Some(largs) ~ _  => AB_PNask(pn,largs) } 

  def rule_primitive : Parser[AB_Primitive] =
     "tellr("~ idLC ~ ")" ^^ {
            case _ ~ rn ~ _  => AB_RTell(rn) } |
     "askr("~ idLC ~ ")" ^^ {
            case _ ~ rn ~ _ => AB_RAsk(rn) } |
     "getr("~ idLC ~ ")" ^^ {
            case _ ~ rn ~ _ => AB_RGet(rn) } |
     "naskr("~ idLC ~ ")" ^^ {
            case _ ~ rn ~ _ => AB_RNask(rn) } 

  def scene_primitive : Parser[AB_Primitive] =
      "draw_scene(" ~ idLC ~ ")" ^^ {
            case _  ~ s ~ _ => AB_Draw_Scene(s) } |
      "place_at("~idLC~","~idLC~","~stInfo~","~stInfo~")" ^^ {
            case _ ~ w ~ _ ~ s ~ _ ~ x ~ _ ~ y ~ _ => AB_Place_Widget(w,s,x,y) } |
      "move_to("~idLC~","~idLC~","~stInfo~","~stInfo~")" ^^ {
            case _ ~ w ~ _ ~ s ~ _ ~ x ~ _ ~ y ~ _  => AB_Widget_Moveto(w,s,x,y) } |
      "hide("~idLC~","~idLC~")" ^^ {
            case _ ~ w ~ _ ~ s ~ _  => AB_Hide_Widget(w,s) } |
      "show("~idLC~","~idLC~")" ^^ {
            case _ ~ w ~ _ ~ s ~ _  => AB_Show_Widget(w,s) } |
      "layer("~idLC~","~idLC~","~idLC~")" ^^ {
            case _ ~ w ~ _ ~ s ~ _ ~ l ~ _ => AB_Layer_Widget(w,s,l) } |
      "att(" ~ stInfo ~ "," ~ idLC ~ "," ~ idLC ~ "," ~ stInfo ~")" ^^ {
            case _ ~ x ~ _ ~ w ~ _ ~ s ~ _ ~ v ~ _ => AB_Att_Widget(x,w,s,v) } 

   def simple_primitive : Parser[AB_Primitive] =
       basic_primitive | multi_primitive | proc_primitive | rule_primitive | scene_primitive


/*          AnimBach agents                   */
  
  val opChoice  : Parser[String] = "+"
  val opPara    : Parser[String] = "||"
  val opSeq     : Parser[String] = ";"


  def primitive : Parser[AB_AST_Primitive]   = 
      basic_primitive ^^ {
            case bp => AB_AST_Primitive(bp,AB_Prim_Type_Basic()) } |
      multi_primitive ^^ {
            case mp => AB_AST_Primitive(mp,AB_Prim_Type_Multi()) } |
      proc_primitive ^^ {
            case pp => AB_AST_Primitive(pp,AB_Prim_Type_Proc()) } |
      rule_primitive ^^ {
            case rp => AB_AST_Primitive(rp,AB_Prim_Type_Rule()) } |
      scene_primitive ^^ {
            case sp => AB_AST_Primitive(sp,AB_Prim_Type_Scene()) } 

  def agent = compositionChoice

  def compositionChoice : Parser[AB_AG] = compositionPara~rep(opChoice~compositionChoice) ^^ {
        case ag ~ List() => ag
        case agi ~ List(op~agii)  => AB_AST_CHOICE_Agent(agi,agii) }

  def compositionPara : Parser[AB_AG] = compositionSeq~rep(opPara~compositionPara) ^^ {
        case ag ~ List() => ag
        case agi ~ List(op~agii)  => AB_AST_PARA_Agent(agi,agii) }

  def compositionSeq : Parser[AB_AG] = simpleAgent~rep(opSeq~compositionSeq) ^^ {
        case ag ~ List() => ag
        case agi ~ List(op~agii)  => AB_AST_SEQ_Agent(agi,agii) }

  def simpleAgent : Parser[AB_AG] = 
        primitive | procedureCall | parenthesizedAgent | sumAgent | condAgent | listPrimitives 

  def listPrimitives : Parser[AB_AG] =
        "|[" ~ rep1sep(primitive, ";") ~ "]|" ^^ {
            case _ ~ lprim ~ _ => AB_AST_List_Primitive(lprim) }

  def parenthesizedAgent : Parser[AB_AG] = "("~>agent<~")"

  def listVarInSet : Parser[List[AB_VAR_IN_SET]] = rep1sep(varInSet,",")
  
  def sumAgent : Parser[AB_AG] = "sum" ~ listVarInSet ~ ":" ~ agent ^^ {
         case _ ~ lv ~ _  ~ ag => AB_AST_GEN_CHOICE_Agent(lv,ag) } 

  def condAgent : Parser[AB_AG] =  ifThenElseAgent | ifThenAgent

  def ifThenAgent : Parser[AB_AG] = abCond ~ "->" ~ agent ^^ {
         case c ~ _ ~ ag => AB_IF_THEN_Agent(c,ag) }

  def ifThenElseAgent : Parser[AB_AG] = abCond ~ "->" ~ agent ~ "<>" ~ agent ^^ {
         case c ~ _ ~ agi ~ _ ~ agii => AB_IF_THEN_ELSE_Agent(c,agi,agii) }



  /*          Procedure definitions             */


  def largsDefProc : Parser[List[(String,String)]] = "("~rep1sep(argDefProc,",")~")" ^^ {
      case _ ~ largs ~ _ => largs }

  def argDefProc   : Parser[(String,String)] = idLC~":"~idUC ^^ {
      case varAP ~_ ~ setAP => (varAP,setAP) }

  def procedureName : Parser[(String,List[(String,String)])] = idUC ~ opt(largsDefProc) ^^ {
      case pn ~ None => (pn,List[(String,String)]())
      case pn ~ Some(largs) => (pn,largs) }


  def procedureBody : Parser[AB_AG] = agent

  def procedureDecl : Parser[AB_Proc_Def] = procedureName~"="~procedureBody~"." ^^ {
       case pnla ~ _ ~ pbody ~ _ =>  AB_Proc_Def(pnla._1,pnla._2,pbody)  }

  def procDecls : Parser[List[AB_Abs_Proc]] = "proc"~rep1(procedureDecl) ^^ {
       case _ ~ pDecls => pDecls }


  def procedureCall : Parser[AB_AG] = idUC~opt(stInfoArgs) ^^ {
         case pn ~ None => AB_AST_Proc_Call(pn,List[AB_SI_ELM]()) 
	 case pn ~ Some(largs) => AB_AST_Proc_Call(pn,largs) }


  /*          Temporal formulae                */

  def ab_sf_exp : Parser[AB_STORE_FORMULA_EXPRESSION] =
       idInt ^^ {
            case i => AB_STR_F_INT(i) } |
       "#" ~ stInfo ^^ {
            case _ ~ s => AB_STR_F_SINFO(s) }

  def ab_store_form : Parser[AB_STORE_FORMULA] = orStoreForm

  def orStoreForm : Parser[AB_STORE_FORMULA] = andStoreForm ~ rep(op_or ~ orStoreForm) ^^ {
       case f ~ List() => f
       case fi ~ List(op~fii) => STORE_COMPII_FORMULA(op,fi,fii) }

  def andStoreForm : Parser[AB_STORE_FORMULA] = simpleStoreForm ~ rep(op_and ~ andStoreForm) ^^ {
       case f ~ List() => f
       case fi ~ List(op~fii) => STORE_COMPII_FORMULA(op,fi,fii) }

  def simpleStoreForm : Parser[AB_STORE_FORMULA] =
       trueStoreForm | falseStoreForm | primitiveStoreForm | negStoreForm | parenthesizedStoreForm

  def trueStoreForm : Parser[AB_STORE_FORMULA] = "true" ^^ {
       case _ => STORE_TRUE_FORMULA() }
       
  def falseStoreForm : Parser[AB_STORE_FORMULA] = "false" ^^ {
       case _ => STORE_FALSE_FORMULA() }
       
  def primitiveStoreForm: Parser[AB_STORE_FORMULA] = ab_sf_exp ~ relop ~ ab_sf_exp ^^ {
       case expi ~ op ~ expii =>  STORE_ATOMIC_FORMULA(op,expi,expii) }

   def negStoreForm: Parser[AB_STORE_FORMULA] = op_neg ~ ab_store_form ^^ {
       case op~f => STORE_COMPI_FORMULA(op,f) }

   def parenthesizedStoreForm: Parser[AB_STORE_FORMULA] = "("~>ab_store_form<~")"

   def ab_ltl_form: Parser[AB_TEMP_FORMULA] =
       ab_store_form ~ opt("Until" ~ ab_ltl_form) ^^ {
           case f ~ None =>  AB_LTL_STORE(f)
	   case fi ~ Some(_ ~ fii) => AB_LTL_UNTIL(fi,fii) } | 
       "Next" ~ ab_ltl_form ^^ {
           case _ ~ f =>  AB_LTL_NEXT(f) } |
       "Reach" ~ ab_store_form ^^ {
           case _ ~ f => AB_LTL_REACH(f) }

}

class AnimBachSimulParser extends AnimBachParsers {

  def parse_primitive(prim: String) = parseAll(simple_primitive,prim) match {
        case Success(result, _) => result
        case failure : NoSuccess => { throw new IllegalStateException("Error in parsing primitive: " + failure.msg) }
  }

  def parse_agent(ag: String) = parseAll(agent,ag) match {
        case Success(result, _) => result
        case failure : NoSuccess => { throw new IllegalStateException("Error in parsing agent: " + failure.msg) }
  }

  def parse_procedure(ag: String) = parseAll(procDecls,ag) match {
        case Success(result, _) => result
        case failure : NoSuccess => { throw new IllegalStateException("Error in parsing procedure declarations: " + failure.msg) }
  }

  def parse_set_defns(ag: String) = parseAll(setSpec,ag) match {
        case Success(result, _) => result
        case failure : NoSuccess => { throw new IllegalStateException("Error in parsing set definitions: " + failure.msg) }
  }

  def parse_scene_defns(ag: String) = parseAll(sceneSpec,ag) match {
        case Success(result, _) => result
        case failure : NoSuccess => { throw new IllegalStateException("Error in parsing scene definitions: " + failure.msg) }
  }

  def parse_stinfo(ag: String) = parseAll(stInfo,ag) match {
        case Success(result, _) => result
        case failure : NoSuccess => { throw new IllegalStateException("Error in parsing st info: " + failure.msg) }
  }

  def parse_var_stinfo(ag: String) = parseAll(stLVarInfo,ag) match {
        case Success(result, _) => result
        case failure : NoSuccess => { throw new IllegalStateException("Error in parsing list of filters: " + failure.msg) }
  }

  def parse_prgm(ag: String) = parseAll(abprgm,ag) match {
        case Success(result, _) => result
        case failure : NoSuccess => { throw new IllegalStateException("Error in parsing program: " + failure.msg) }
  }

  def parse_formula(ag: String) = parseAll(ab_ltl_form,ag) match {
        case Success(result, _) => result
        case failure : NoSuccess => { throw new IllegalStateException("Error in parsing formula: " + failure.msg) }
	}

}


object AgParser extends AnimBachParsers {
  def main(args: Array[String]) {
    println("input : "+ args(0))
    val res = parseAll(agent, args(0)) match {
         case Success(result, _) => result
         case failure : NoSuccess => scala.sys.error(failure.msg)
   }
    println("output : "+res)
  }
}

import scala.io.Source
object AgPrgmParserAsScript extends AnimBachParsers {
  def main(args: Array[String]) {
     if (args.length == 1) {
        // println("input : "+ args(0))
	println("File received and being parsed ...")
        val fileContents = Source.fromFile(args(0)).getLines.mkString
        val res = parseAll(abprgm, fileContents) match {
            case Success(result, _) => result
            case failure : NoSuccess => scala.sys.error(failure.msg)
        }
        println("output : "+res)
     } else {
        println("I didn't get the file")
     }
  }
}

object AgPrgmParser extends AnimBachParsers {
  def main(args: Array[String]) {
    println("input : "+ args(0))
    val res = parseAll(abprgm, args(0)) match {
         case Success(result, _) => result
         case failure : NoSuccess => scala.sys.error(failure.msg)
   }
    println("output : "+res)
  }
}

object AbSetParser extends AnimBachParsers {
  def main(args: Array[String]) {
    println("input : "+ args(0))
    val res = parseAll(setSpec, args(0)) match {
         case Success(result, _) => result
         case failure : NoSuccess => scala.sys.error(failure.msg)
   }
    println("output : "+res)
  }
}



class PrettyPrinterSetDefns {

   def translateSetDfn(setDfn: AB_SET):String = {

      setDfn match {
         case AB_SET_DEF(setName,setElms) => setName + " = {" + setElms.mkString(" ,") + "}" + "\n"
     }

   }

   def translate(setDfns: List[AB_SET]): String = {

      setDfns match {

         case Nil => ""
	 case setDfn :: tailSetDefns => translateSetDfn(setDfn) + translate(tailSetDefns)

      }
      
   }

}

class PrettyPrinterSceneDefns {

   def translate(sceneDfns: List[AB_SCENE_DEFN]): String = {
       (sceneDfns.map(x=>x.ab_toString)).mkString(". "+"\n")
   }

}


class PrettyPrinterRulesDefns {

   def translate(ruleDfns: List[AB_GEN_RULE]): String = {
       (ruleDfns.map(x=>x.ab_toString)).mkString(". "+ "\n")
   }

}

class PrettyPrinterProcsDefns {

   def translateProcDfn(procDfn: AB_Abs_Proc):String = {

       procDfn match {
         case AB_Proc_Def(pn,pargs,pb) => {
	      var s = ""
	      if (pargs == Nil) { s = pn } else { s = pn + "(" + pargs.mkString(",") + ")" }
	      s + " = " + pb.ab_toString + " ." + "\n" }
       }
   }

   def translate(procDfns: List[AB_Abs_Proc]): String = {

      procDfns match {

         case Nil => ""
	 case procDfn :: tailProcDefns => translateProcDfn(procDfn) + translate(tailProcDefns)

      }
      
   }

}


class PrettyPrinterEqnsDefns {

   def translateEqnDfn(eqnDfn: AB_MAP_DEFN):String = {

       eqnDfn match {
         case AB_MAP_EQ(e,f) => {
           e.ab_toString + " = " + f.ab_toString + "\n" }
       }
   }

   def translate(eqnDfns: List[AB_MAP_DEFN]): String = {

      eqnDfns match {

         case Nil => ""
	 case eqnDfn :: tailEqnDefns => translateEqnDfn(eqnDfn) + translate(tailEqnDefns)

      }
      
   }

}


class PrettyPrinterFormula {

   def translate(f:AB_TEMP_FORMULA): String = { f.ab_toString }
   
}

