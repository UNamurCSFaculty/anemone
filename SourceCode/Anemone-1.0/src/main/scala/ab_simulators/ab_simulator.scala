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

package ab_simulators

import ab_data._
import ab_blackboard._
import ab_scene._

import scala.util.Random

class ABSimulExec(var current_agent: AB_AG, var bb: ABBlackboard, var mySetScenes: ABSetOfScenes) {

   var simulType:AB_SIMUL_TYPE=AB_INTERACTIVE_SIMUL()

   def simulAsType(t: AB_SIMUL_TYPE) {
      simulType = t
   }

   val db_random_choice = new Random()

   def stinfo_simplify(s:AB_SI_ELM) = {
       val eqns = bb.listEqns
       val xs = eqns.map(x=> { x match { case AB_MAP_EQ(sti,stii) => sti }})
       val ts = eqns.map(x=> { x match { case AB_MAP_EQ(sti,stii) => stii }})
       s.ab_simplify(xs,ts)
   }

   def l_stinfo_simplify(lsi:List[AB_SI_ELM]) = {
       lsi.map(x=>stinfo_simplify(x))
   }


   def ag_simplify(ag:AB_AG) = {
       val eqns = bb.listEqns
       val xs = eqns.map(x=> { x match { case AB_MAP_EQ(sti,stii) => sti }})
       val ts = eqns.map(x=> { x match { case AB_MAP_EQ(sti,stii) => stii }})
       ag.ab_simplify(xs,ts)
   }

   def ab_eval_cond_elm(relop:String,i:Int,j:Int):Boolean = {
      if      ( relop == "=" ) { i==j }
      else if ( relop == "<" ) { i<j  }
      else if ( relop == "<=") { i<=j }
      else if ( relop == ">" ) { i>j  }
      else if ( relop == ">=") { i>=j }
      else { false }
   }

   def ab_eval_cond(c: AG_COND): Boolean = {
      c match {
          case COND_ATOMIC(relop,argi,argii) => {
             if ( argi.atomicSiElm && argii.atomicSiElm ) {
                val a = argi.functorSiElm
	        val b = argii.functorSiElm
	        val i = bb.ordDecl(a)
	        val j = bb.ordDecl(b)
	        ab_eval_cond_elm(relop,i,j)
             } else {
	        false
	     }
	   }
           case COND_COMPI(relop,cond) => {
	     if (relop == "!") { !ab_eval_cond(cond)
	     } else { false }
	   }
           case COND_COMPII(relop,condi,condii) => {
	      val c = ab_eval_cond(condi)
	      val d = ab_eval_cond(condii)
	      if      ( relop == "&" )  { c && d }
	      else if ( relop == "|" )  { c || d }
	      else { false }
	   }
       }
   }

   def anim_args_simplify(args:List[AB_SI_ELM]):List[AB_SI_ELM] = {
       val eqns = bb.listEqns
       val xs = eqns.map(x=> { x match { case AB_MAP_EQ(sti,stii) => sti }})
       val ts = eqns.map(x=> { x match { case AB_MAP_EQ(sti,stii) => stii }})
       args.map(s=>s.ab_simplify(xs,ts))
   }

   def run_unselect(ab_ag: AB_AG): AB_AG = {
      ab_ag match {

         case AB_AST_Empty_Agent() => AB_AST_Empty_Agent()

         case AB_AST_Primitive(prim,prim_type) =>  AB_AST_Primitive(prim,prim_type)

         case AB_AST_Proc_Call(pn,args) => AB_AST_Proc_Call(pn,args)

         case AB_Exec_AST_Primitive(prim,prim_type,pp) =>  AB_AST_Primitive(prim,prim_type) 

         case AB_Exec_AST_Proc_Call(pn,args,pp) => AB_AST_Proc_Call(pn,args)

         case AB_AST_SEQ_Agent(ag_i,ag_ii) =>  AB_AST_SEQ_Agent(run_unselect(ag_i),run_unselect(ag_ii))
         case AB_AST_PARA_Agent(ag_i,ag_ii) =>  AB_AST_PARA_Agent(run_unselect(ag_i),run_unselect(ag_ii))	 
         case AB_AST_CHOICE_Agent(ag_i,ag_ii) =>  AB_AST_CHOICE_Agent(run_unselect(ag_i),run_unselect(ag_ii))	 
         case AB_AST_GEN_CHOICE_Agent(varsInSets,ag) => AB_AST_GEN_CHOICE_Agent(varsInSets,run_unselect(ag))
         case AB_Exec_AST_GEN_CHOICE_Agent(varsInSets,ag,pp) => AB_AST_GEN_CHOICE_Agent(varsInSets,run_unselect(ag))	 

         case AB_AST_List_Primitive(lp) =>  AB_AST_List_Primitive(lp)
         case AB_Exec_AST_List_Primitive(prim,lprim,pp) =>  AB_AST_List_Primitive(prim::lprim)	 

         case AB_IF_THEN_Agent(c,ag) =>  AB_IF_THEN_Agent(c,run_unselect(ag))

         case AB_IF_THEN_ELSE_Agent(c,ag_i,ag_ii) =>  AB_IF_THEN_ELSE_Agent(c,run_unselect(ag_i),run_unselect(ag_ii)) 
       }
   }


   def run_selected(ab_ag: AB_AG,path:List[Int]): AB_AG = {
      ab_ag match {

         case AB_AST_Empty_Agent() => AB_AST_Empty_Agent()

         case AB_AST_Primitive(prim,prim_type) =>  AB_AST_Primitive(prim,prim_type)

         case AB_Exec_AST_Primitive(prim,prim_type,pp) => {
              exec_primitive(prim,prim_type)
              AB_AST_Empty_Agent() }

         case AB_AST_Proc_Call(pn,args) => AB_AST_Proc_Call(pn,args)

         case AB_Exec_AST_Proc_Call(pn,args,pp) => {
	      val ag=exec_proc_call(pn,args)
	      ag }

         case AB_AST_List_Primitive(lp) =>  AB_AST_List_Primitive(lp)

         case AB_Exec_AST_List_Primitive(prim,lprim,pp) =>  {
              prim match {
	         case AB_AST_Primitive(pprim,pprim_type) => {
                       exec_primitive(pprim,pprim_type) } }
              lprim match {
	         case Nil =>  AB_AST_Empty_Agent()
		 case _ =>  AB_AST_List_Primitive(lprim) }
         }

         case AB_AST_SEQ_Agent(ag_i,ag_ii) =>  {
             val new_ag = run_selected(ag_i,path.tail)
             if ( new_ag == AB_AST_Empty_Agent() ) { ag_ii }
             else { AB_AST_SEQ_Agent(new_ag,ag_ii) }
         }

         case AB_AST_PARA_Agent(ag_i,ag_ii) => {
            if ( path.head == 1 ) { 
               val new_ag_i = run_selected(ag_i,path.tail)
               val new_ag_ii = run_unselect(ag_ii)
               if ( new_ag_i == AB_AST_Empty_Agent() ) { new_ag_ii }
               else { AB_AST_PARA_Agent(new_ag_i,new_ag_ii) } }
            else {
               val new_ag_i = run_unselect(ag_i)
               val new_ag_ii = run_selected(ag_ii,path.tail)
               if ( new_ag_ii == AB_AST_Empty_Agent() ) { new_ag_i }
               else { AB_AST_PARA_Agent(new_ag_i,new_ag_ii) } }
          }

         case AB_AST_CHOICE_Agent(ag_i,ag_ii) => {
            if ( path.head == 1 ) { run_selected(ag_i,path.tail) }
            else { run_selected(ag_ii,path.tail) }
          }

         case AB_AST_GEN_CHOICE_Agent(varsInSets,ag) => AB_AST_GEN_CHOICE_Agent(varsInSets,ag)

         case AB_Exec_AST_GEN_CHOICE_Agent(varsInSets,ag,pp) => {
 	      val ag_bc=exec_gen_sum(varsInSets,ag)
              ag_bc
	 }

         case AB_IF_THEN_Agent(c,ag) => {
            if ( path.head == 1 ) { run_selected(ag,path.tail) }
            else { AB_IF_THEN_Agent(c,ag) }
          }

         case AB_IF_THEN_ELSE_Agent(c,ag_i,ag_ii) => {
            if ( path.head == 1 ) { run_selected(ag_i,path.tail) }
            else { run_selected(ag_ii,path.tail) }
          }

       }
   }


   def cross_product_set_elms(lst: List[List[String]],acc:List[String]): List[List[String]] = {
      lst match {
        case head :: Nil => head.map(_ :: acc)
        case head :: tail => head.flatMap(x => cross_product_set_elms(tail,x :: acc))
        case Nil => List[List[String]]()
      }
   }

   def gen_choice_instances(varsInSets: List[AB_GEN_VAR_IN_SET], ag: AB_AG): List[AB_AG] = {
         varsInSets match {
	   case Nil => List(ag)
	   case x :: xs => {
             val varsInSetsAsString = varsInSets.map(x=>x.ab_varN)
             val varsInSetsAsSiElms = varsInSetsAsString.map(x=>AB_SI_ATOMIC(x))
             val list_set_names =  varsInSets.map(x=>x.ab_sname)
             val ll_selms = list_set_names.map(sn=>bb.setMembers(sn))
             val llElmsInSetsAsString = cross_product_set_elms(ll_selms,List[String]())
             var lagInst = List[AB_AG]()
             var eAsSiElms = List[AB_SI_ELM]()
             for (e <- llElmsInSetsAsString) {
               eAsSiElms = e.map(x=>AB_SI_ATOMIC(x))
               lagInst = ag.ab_substitute(varsInSetsAsSiElms,eAsSiElms) :: lagInst
             }
             lagInst
           }
	}
   }

   def partition_lag(l_ag:List[AB_AG]): (List[AB_AG],List[AB_AG]) = {
      val lg = (l_ag.size)/2
      val l_ag_i = l_ag.take(lg)
      val l_ag_ii = l_ag.drop(lg)
      (l_ag_i,l_ag_ii)
   }
   
   def make_big_choice(l_ag:List[AB_AG]):AB_AG = {
      l_ag match {
	case x::xs => {
	  xs match {
	    case Nil => x
	    case y::ys => {
               val p = partition_lag(l_ag)
	       val l_ag_i = p._1
	       val l_ag_ii = p._2
               val ag_i = make_big_choice(l_ag_i)
               val ag_ii = make_big_choice(l_ag_ii)
               AB_AST_CHOICE_Agent(ag_i,ag_ii)
            }
          }
        }
      }
   }

   def run_one(ab_ag: AB_AG):(Boolean,AB_AG) = {
      ab_ag match {

         case AB_AST_Primitive(prim,prim_type) => 
            {  if (exec_primitive(prim,prim_type)) { (true,AB_AST_Empty_Agent()) }
               else { (false,ab_ag) }
            }

         case AB_AST_Proc_Call(pn,args) => 
	    { if (procDefined(pn)) { (true,exec_proc_call(pn,args)) }
	      else { (false,ab_ag) }
	    }

         case AB_AST_List_Primitive(lprim) =>  {
	    lprim match {

                case Nil => { (true,AB_AST_Empty_Agent()) }

                case prim :: lprim_rem => {

                   prim match {
	              case AB_AST_Primitive(pprim,pprim_type) => {
                         if (exec_primitive(pprim,pprim_type)) {
			   lprim_rem match {
			      case Nil => (true,AB_AST_Empty_Agent())
			      case _ => (true, AB_AST_List_Primitive(lprim_rem))
			   }
			 } else {
			   (false,ab_ag)
			 } 
                      }
		    }
		 }
	   }
         }
                      

         case AB_AST_SEQ_Agent(ag_i,ag_ii) =>
            {  run_one( ag_i) match
                  { case (false,_)         => (false,ab_ag)
                    case (true,AB_AST_Empty_Agent())   => (true,ag_ii)                 
                    case (true,ag_cont)   => (true,AB_AST_SEQ_Agent(ag_cont,ag_ii))
                  }
            }

         case AB_AST_PARA_Agent(ag_i,ag_ii) =>
            {  var branch_choice = db_random_choice.nextInt(2) 
               if (branch_choice == 0) 
                 { run_one( ag_i ) match
                      { case (false,_)         => 
                          { run_one( ag_ii ) match
                              { case (false,_)    => (false,ab_ag)
                                case (true,AB_AST_Empty_Agent())   => (true,ag_i)                 
                                case (true,ag_cont)   => (true,AB_AST_PARA_Agent(ag_i,ag_cont))
                              }
                          }
                        case (true,AB_AST_Empty_Agent())   => (true,ag_ii)                 
                        case (true,ag_cont)   => (true,AB_AST_PARA_Agent(ag_cont,ag_ii))
                      }
                  }
               else
                 { run_one( ag_ii ) match
                      { case (false,_)         => 
                          { run_one( ag_i ) match
                              { case (false,_)    => (false,ab_ag)
                                case (true,AB_AST_Empty_Agent())   => (true,ag_ii)                 
                                case (true,ag_cont)   => (true,AB_AST_PARA_Agent(ag_cont,ag_ii))
                              }
                          }
                        case (true,AB_AST_Empty_Agent())   => (true,ag_i)                 
                        case (true,ag_cont)   => (true,AB_AST_PARA_Agent(ag_i,ag_cont))
                      }
                  }             
            }


         case AB_AST_CHOICE_Agent(ag_i,ag_ii) =>
            {  var branch_choice = db_random_choice.nextInt(2) 
               if (branch_choice == 0) 
                 { run_one( ag_i ) match
                      { case (false,_)         => 
                          { run_one( ag_ii ) match
                              { case (false,_)    => (false,ab_ag)
                                case (true,AB_AST_Empty_Agent())   => (true,AB_AST_Empty_Agent())                 
                                case (true,ag_cont)   => (true,ag_cont)
                              }
                          }
                        case (true,AB_AST_Empty_Agent())   => (true,AB_AST_Empty_Agent())                 
                        case (true,ag_cont)   => (true,ag_cont)
                      }
                  }
               else
                 { run_one( ag_ii ) match
                      { case (false,_)         => 
                          { run_one( ag_i ) match
                              { case (false,_)    => (false,ab_ag)
                                case (true,AB_AST_Empty_Agent())   => (true,AB_AST_Empty_Agent())                 
                                case (true,ag_cont)   => (true,ag_cont)
                              }
                          }
                        case (true,AB_AST_Empty_Agent())   => (true,AB_AST_Empty_Agent())                 
                        case (true,ag_cont)   => (true,ag_cont)
                      }
                  }             
            }


         case AB_AST_GEN_CHOICE_Agent(varsInSets,ag) => {
           val lagInst = gen_choice_instances(varsInSets,ag)
	   (true,make_big_choice(lagInst))
         }

         case AB_IF_THEN_Agent(c,ag) => {
           if (ab_eval_cond(c)) {
                 run_one(ag) match 
		   { case (false,_) => (false,ab_ag)
		     case (true,AB_AST_Empty_Agent())   => (true,AB_AST_Empty_Agent())                 
                     case (true,ag_cont)   => (true,ag_cont)
                   }
           } else {
              (false,ab_ag)
           }
         }

         case AB_IF_THEN_ELSE_Agent(c,agi,agii) => {
           if (ab_eval_cond(c)) {
                 run_one(agi) match
		   { case (false,_) => (false,ab_ag)
		     case (true,AB_AST_Empty_Agent())   => (true,AB_AST_Empty_Agent())                 
                     case (true,ag_cont)   => (true,ag_cont)
                   }
           } else {
                 run_one(agii) match 
		   { case (false,_) => (false,ab_ag)
		     case (true,AB_AST_Empty_Agent())   => (true,AB_AST_Empty_Agent())                 
                     case (true,ag_cont)   => (true,ag_cont)
                   }
           }
         }
    }
   }

   def exec_primitive(prim:AB_Primitive,prim_type:AB_PRIMITIVE_TYPE):Boolean = {
       prim_type match {
          case AB_Prim_Type_Basic() => exec_basic_primitive(prim.asInstanceOf[AB_Basic_Primitive])
	  case AB_Prim_Type_Multi() => exec_multi_primitive(prim.asInstanceOf[AB_Multi_Primitive])
	  case AB_Prim_Type_Proc() => exec_proc_primitive(prim.asInstanceOf[AB_Proc_Primitive])
	  case AB_Prim_Type_Rule() => exec_rule_primitive(prim.asInstanceOf[AB_Rule_Primitive])
	  case AB_Prim_Type_Scene() => exec_scene_primitive(prim.asInstanceOf[AB_Scene_Primitive])
       }
   }

   def exec_basic_primitive(prim:AB_Basic_Primitive):Boolean = {
       prim match {
          case AB_Tell(stinfo) => bb.tell(stinfo_simplify(stinfo))
          case AB_Ask(stinfo) => bb.ask(stinfo_simplify(stinfo))
          case AB_Nask(stinfo) => bb.nask(stinfo_simplify(stinfo))
          case AB_Get(stinfo) => bb.get(stinfo_simplify(stinfo))
       }
   }

   def exec_multi_primitive(prim:AB_Multi_Primitive):Boolean = {
       // To be implemented
       true
   }

   def exec_proc_primitive(prim:AB_Proc_Primitive):Boolean = {
       prim match {
         case AB_PTell(pn,args) => {
	      val args_simplified = l_stinfo_simplify(args)
	      val proc_call_ast = AB_AST_Proc_Call(pn,args_simplified)
	      val proc_call_string = AB_AST_Proc_Call(pn,args_simplified).ab_toString
              bb.tellp(proc_call_string,proc_call_ast,simulType,mySetScenes)
	 }
         case AB_PAsk(pn,args) => {
	      val args_simplified = l_stinfo_simplify(args)
	      val proc_call_string = AB_AST_Proc_Call(pn,args_simplified).ab_toString
              bb.askp(proc_call_string)
         }	 
         case AB_PNask(pn,args) => {
	      val args_simplified = l_stinfo_simplify(args)
	      val proc_call_string = AB_AST_Proc_Call(pn,args_simplified).ab_toString
              bb.naskp(proc_call_string)
         }	 
         case AB_PGet(pn,args) => {
	      val args_simplified = l_stinfo_simplify(args)
	      val proc_call_string = AB_AST_Proc_Call(pn,args_simplified).ab_toString
              bb.getp(proc_call_string)
	 }
       }
   }

   def exec_rule_primitive(prim:AB_Rule_Primitive):Boolean = {
      prim match {
         case AB_RTell(rn) => bb.tellr(rn)
	 case AB_RAsk(rn) => bb.askr(rn)
	 case AB_RGet(rn) => bb.getr(rn)	 
	 case AB_RNask(rn) => bb.naskr(rn)	 
      }
   }

   def exec_scene_primitive(prim:AB_Scene_Primitive):Boolean = {
       prim match {
          case AB_Draw_Scene(sn) => {
	       mySetScenes.draw_scene(sn,bb)
	       true }
          case AB_Place_Widget(wn,sn,x,y) => {
	       val x_as_double = (stinfo_simplify(x).ab_toString).toDouble
	       val y_as_double = (stinfo_simplify(y).ab_toString).toDouble	       
	       mySetScenes.place_at(wn,sn,x_as_double,y_as_double)
	       true }
          case AB_Widget_Moveto(wn,sn,x,y) => {
	       val x_as_double = (stinfo_simplify(x).ab_toString).toDouble
	       val y_as_double = (stinfo_simplify(y).ab_toString).toDouble	       
	       mySetScenes.move_to(wn,sn,x_as_double,y_as_double)
	       true }
          case AB_Hide_Widget(wn,sn) => {
	       mySetScenes.hide(wn,sn)
	       true }
          case AB_Show_Widget(wn,sn) => {
	       mySetScenes.show(wn,sn)
	       true }
          case AB_Layer_Widget(wn,sn,l) => {
	       mySetScenes.layer(wn,sn,l)
	       true }
          case AB_Att_Widget(att,wn,sn,v) => {
               att match {
	         case AB_SI_ATOMIC(att_as_string) => {
   	            mySetScenes.att(att_as_string,wn,sn,stinfo_simplify(v),bb) }
		 case _ => {  }
	       }
	       true }
      }
   }	  

   def exec_proc_call(pn:String,args:List[AB_SI_ELM]):AB_AG = {
       val formal_args = bb.procFormalArgs(pn)
       val ag = bb.procBody(pn)
       val agr = ag.ab_substitute(formal_args,args)
       val agrs = ag_simplify(agr)
       agrs
   }

   def procDefined(pn:String) = { bb.procDefined(pn) }
   def procBody(pn:String) = { bb.procBody(pn) }

   def exec_gen_sum(varsInSets: List[AB_GEN_VAR_IN_SET], ag: AB_AG): AB_AG = {
           val lagInst = gen_choice_instances(varsInSets,ag)
	   make_big_choice(lagInst)
   }
   
   def test_exec_primitive(prim:AB_Primitive,prim_type:AB_PRIMITIVE_TYPE,bb:ABBlackboard):Boolean = {
       prim_type match {
          case AB_Prim_Type_Basic() => test_exec_basic_primitive(prim.asInstanceOf[AB_Basic_Primitive],bb)
	  case AB_Prim_Type_Multi() => test_exec_multi_primitive(prim.asInstanceOf[AB_Multi_Primitive],bb)
	  case AB_Prim_Type_Proc() => test_exec_proc_primitive(prim.asInstanceOf[AB_Proc_Primitive],bb)
	  case AB_Prim_Type_Rule() => test_exec_rule_primitive(prim.asInstanceOf[AB_Rule_Primitive],bb)
	  case AB_Prim_Type_Scene() => test_exec_scene_primitive(prim.asInstanceOf[AB_Scene_Primitive],bb)
       }
   }

   def test_exec_basic_primitive(prim:AB_Basic_Primitive,bb:ABBlackboard):Boolean = {
       prim match {
          case AB_Tell(stinfo) => bb.test_tell(stinfo_simplify(stinfo))
          case AB_Ask(stinfo) => bb.test_ask(stinfo_simplify(stinfo))
          case AB_Nask(stinfo) => bb.test_nask(stinfo_simplify(stinfo))
          case AB_Get(stinfo) => bb.test_get(stinfo_simplify(stinfo))
       }
   }

   def test_exec_multi_primitive(prim:AB_Multi_Primitive,bb:ABBlackboard):Boolean = {
       // To be implemented
       true
   }

   def test_exec_proc_primitive(prim:AB_Proc_Primitive,bb:ABBlackboard):Boolean = {
       prim match {
         case AB_PTell(pn,args) => {
	      val args_simplified = l_stinfo_simplify(args)
	      val proc_call_ast = AB_AST_Proc_Call(pn,args_simplified)
	      val proc_call_string = AB_AST_Proc_Call(pn,args_simplified).ab_toString
              bb.test_tellp(proc_call_string,proc_call_ast,simulType,mySetScenes)
	 }
         case AB_PAsk(pn,args) => {
	      val args_simplified = l_stinfo_simplify(args)	 
 	      val proc_call_ast = AB_AST_Proc_Call(pn,args_simplified)
	      val proc_call_string = AB_AST_Proc_Call(pn,args_simplified).ab_toString
              bb.test_askp(proc_call_string)
         }
         case AB_PNask(pn,args) => {
	      val args_simplified = l_stinfo_simplify(args)	 
 	      val proc_call_ast = AB_AST_Proc_Call(pn,args_simplified)
	      val proc_call_string = AB_AST_Proc_Call(pn,args_simplified).ab_toString
              bb.test_naskp(proc_call_string)
	 }
         case AB_PGet(pn,args) => {
	      val args_simplified = l_stinfo_simplify(args)	 
 	      val proc_call_ast = AB_AST_Proc_Call(pn,args_simplified)
	      val proc_call_string = AB_AST_Proc_Call(pn,args_simplified).ab_toString
              bb.test_getp(proc_call_string)
	 }
       }
   }

   def test_exec_rule_primitive(prim:AB_Rule_Primitive,bb:ABBlackboard):Boolean = {
      prim match {
         case AB_RTell(rn) => bb.test_tellr(rn)
	 case AB_RAsk(rn) => bb.test_askr(rn)
	 case AB_RGet(rn) => bb.test_getr(rn)	 
	 case AB_RNask(rn) => bb.test_naskr(rn)
      }
   }

   def test_exec_scene_primitive(prim:AB_Scene_Primitive,bb:ABBlackboard):Boolean = {
       // To be implemented
       // See code below
       // Attention several possible scenes
       true
   }

   def ag_first_steps(ab_ag: AB_AG,path:List[Int]):AB_AG = {
      ab_ag match {

         case AB_AST_Empty_Agent() => AB_AST_Empty_Agent()

         case AB_AST_Primitive(ab_prim,ab_prim_type) => 
            {  if (test_exec_primitive(ab_prim,ab_prim_type,bb)) { AB_Exec_AST_Primitive(ab_prim,ab_prim_type,path) }
               else { AB_AST_Primitive(ab_prim,ab_prim_type) }
            }

         case AB_AST_List_Primitive(lprim) =>
	    { lprim match {
	         case Nil => {
		     AB_AST_List_Primitive(lprim) }
		 case prim :: lprim_rem => {
		     prim match { 
		        case AB_AST_Primitive(ab_prim,ab_prim_type) => {
                           if (test_exec_primitive(ab_prim,ab_prim_type,bb)) {
			      AB_Exec_AST_List_Primitive(prim,lprim_rem,path)
			   } else {
			      AB_AST_List_Primitive(lprim)
			   } }
			case _ => AB_AST_List_Primitive(lprim)
                     } }
               }
	     }
		 
	 
         case AB_AST_Proc_Call(pn,args) => AB_Exec_AST_Proc_Call(pn,args,path)

         case AB_AST_SEQ_Agent(ag_i,ag_ii) => AB_AST_SEQ_Agent(ag_first_steps(ag_i,path:::List(1)),ag_ii)

         case AB_AST_PARA_Agent(ag_i,ag_ii) =>  AB_AST_PARA_Agent(ag_first_steps(ag_i,path:::List(1)),ag_first_steps(ag_ii,path:::List(2)))

         case AB_AST_CHOICE_Agent(ag_i,ag_ii) =>  AB_AST_CHOICE_Agent(ag_first_steps(ag_i,path:::List(1)),ag_first_steps(ag_ii,path:::List(2)))

         case AB_AST_GEN_CHOICE_Agent(varsInSets,ag) => AB_Exec_AST_GEN_CHOICE_Agent(varsInSets,ag,path)
	 
         case AB_IF_THEN_Agent(c,ag) =>
	    { if (ab_eval_cond(c)) { AB_IF_THEN_Agent(c,ag_first_steps(ag,path:::List(1))) }
	      else { AB_IF_THEN_Agent(c,ag) }
	    }

         case AB_IF_THEN_ELSE_Agent(c,agi,agii) =>
	    { if (ab_eval_cond(c)) { AB_IF_THEN_ELSE_Agent(c,ag_first_steps(agi,path:::List(1)),agii) }
	      else { AB_IF_THEN_ELSE_Agent(c,agi,ag_first_steps(agii,path:::List(2))) } 
	    }

       }
    }
   
}
