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

import scala.collection.mutable.Map
import scala.collection.mutable.Set
import java.util.Calendar

class search_node
case class empty_node() extends search_node
case class bfs_node(i : Int, ag: AB_AG, store: scala.collection.immutable.Map[AB_SI_ELM,Int], sf: AB_STORE_FORMULA,
                    prev: search_node, lprim: List[AB_AG]) extends search_node

class test_search_node
case class test_bfs_node(ag: AB_AG, sf: AB_STORE_FORMULA)


class ABSimulCheckLTS(var bb: ABBlackboard, var mySetScenes: ABSetOfScenes) {

   type LPRIM = List[AB_AG]
   // intended to have either bb primitive or anim primitive

   var dfs_current_goals_bbs_ltls = List[(AB_AG,scala.collection.immutable.Map[AB_SI_ELM,Int],AB_TEMP_FORMULA)]()

   var bfs_set_goals_bbs_sfs = Set[bfs_node]()
   var bfs_test_set_goals_bbs_sfs = Set[test_bfs_node]()      
   var bfs_list_goals_bbs_sfs = List[bfs_node]()

   var c_gbf : (AB_AG,scala.collection.immutable.Map[AB_SI_ELM,Int],AB_TEMP_FORMULA) = _
   var c_store: scala.collection.immutable.Map[AB_SI_ELM,Int] = scala.collection.immutable.Map.empty ++ bb.storeContents
   var c_init_store: scala.collection.immutable.Map[AB_SI_ELM,Int] = scala.collection.immutable.Map.empty ++ bb.storeContents

   val mc_depth = 5000
   
   def stinfo_simplify(s:AB_SI_ELM) = {
       val eqns = bb.listEqns
       val xs = eqns.map(x=> { x match { case AB_MAP_EQ(sti,stii) => sti }})
       val ts = eqns.map(x=> { x match { case AB_MAP_EQ(sti,stii) => stii }})
       s.ab_simplify(xs,ts)
   }

   def eval_store_form_exp(f : AB_STORE_FORMULA_EXPRESSION) = {

      f match {

          case AB_STR_F_INT(i) => i

          case AB_STR_F_SINFO(s) => {
             val s_simp = stinfo_simplify(s)
	     bb.nb_occ(s_simp)
          }

      } 

   }

   def eval_ltl_store(f: AB_STORE_FORMULA): Boolean = {

      f match {

          case STORE_TRUE_FORMULA() => true

          case STORE_FALSE_FORMULA() => false

          case STORE_ATOMIC_FORMULA(relop,argi,argii) => {
             val a = eval_store_form_exp(argi)
	     val b = eval_store_form_exp(argii)

             if      ( relop == "=" ) { a==b }
             else if ( relop == "<" ) { a<b  }
             else if ( relop == "<=") { a<=b }
             else if ( relop == ">" ) { a>b  }
             else if ( relop == ">=") { a>=b }
             else { false }
          }

          case STORE_COMPI_FORMULA(boolop,arg)=> {
             if (boolop == "!") { ! eval_ltl_store(arg) }
	     else { false }
          }

          case STORE_COMPII_FORMULA(boolop,argi,argii) => {
	     val a = eval_ltl_store(argi)
	     val b = eval_ltl_store(argii)	     

             if      ( boolop == "&") { a && b }
	     else if ( boolop == "|") { a || b }
	     else { false }

          }

      }
   }

   def extend_seq(l_pcp: List[(LPRIM,AB_AG,List[Int])], ag_ii: AB_AG) = {

     l_pcp.map( x => { x match { case (lprim,cont,path) =>
                                   if (cont == AB_AST_Empty_Agent()) { (lprim,ag_ii,List(1):::path) }
				   else { (lprim,AB_AST_SEQ_Agent(cont,ag_ii),List(1):::path) }
                               } })

   }

   def extend_para(l_pcp: List[(LPRIM,AB_AG,List[Int])], ag: AB_AG, i: Int) = {

     l_pcp.map( x => { x match { case (lprim,cont,path) =>

           if (cont == AB_AST_Empty_Agent()) { (lprim,ag,List(i):::path) 
           } else {
	      if (i==1) {
	         ( lprim, AB_AST_PARA_Agent(cont,ag), List(1):::path )
	      } else {
	         ( lprim, AB_AST_PARA_Agent(ag,cont), List(2):::path )	      
              }
	   }
     } } )

   }

   def extend_choice(l_pcp: List[(LPRIM,AB_AG,List[Int])], i: Int) = {

     l_pcp.map( x => { x match { case (lprim,cont,path) =>

           (lprim,cont,List(i):::path)

     } } )

   }




   def lag_first_steps(ag: AB_AG): List[(LPRIM,AB_AG,List[Int])] = {

     ag match {

         case AB_AST_Empty_Agent() => List[(LPRIM,AB_AG,List[Int])]()

         case AB_AST_Primitive(ab_prim,ab_prim_type) => 
            {  if (test_exec_primitive(ab_prim,ab_prim_type,bb)) {
	          List( ( List(AB_AST_Primitive(ab_prim,ab_prim_type)), AB_AST_Empty_Agent(), List[Int]() ) )
               } else {
	          List[(LPRIM,AB_AG,List[Int])]()
               }
            }

         case AB_AST_Proc_Call(pn,args) => {
	    if (procDefined(pn)) {
              val formal_args = bb.procFormalArgs(pn)
              val ag = bb.procBody(pn)
              val agr = ag.ab_substitute(formal_args,args)
              val agrs = ag_simplify(agr)
              lag_first_steps(agrs)
            } else {
              List[(LPRIM,AB_AG,List[Int])]()
	    }
         }

         case AB_AST_SEQ_Agent(ag_i,ag_ii) => {
           val l_pcp = lag_first_steps(ag_i)
	   extend_seq(l_pcp,ag_ii)
         } 

         case AB_AST_PARA_Agent(ag_i,ag_ii) =>  {
           val li = lag_first_steps(ag_i)
           val lii = lag_first_steps(ag_ii)
	   extend_para(li,ag_ii,1) ::: extend_para(lii,ag_i,2)
	 }

         case AB_AST_CHOICE_Agent(ag_i,ag_ii) =>  {
           val li = lag_first_steps(ag_i)
           val lii = lag_first_steps(ag_ii)
	   extend_choice(li,1) ::: extend_choice(lii,2)
         }

         case AB_AST_GEN_CHOICE_Agent(varsInSets,ag) => {
	      val ag_bc=exec_gen_sum(varsInSets,ag)
              lag_first_steps(ag_bc)
         }

         case AB_IF_THEN_Agent(c,ag) => {
            if (ab_eval_cond(c)) {
              extend_choice(lag_first_steps(ag),1)
	    } else {
              List[(LPRIM,AB_AG,List[Int])]()
	    }
	 }

         case AB_IF_THEN_ELSE_Agent(c,agi,agii) => {
	    if (ab_eval_cond(c)) {
               extend_choice(lag_first_steps(agi),1)
	    } else {
               extend_choice(lag_first_steps(agii),2)
            }
	 }


       }
    }
   
   

   def check_lts(i: Int, ag: AB_AG, ltl_form: AB_TEMP_FORMULA):
                                           (Boolean,List[List[Int]],LPRIM) = {

      dfs_current_goals_bbs_ltls = List[(AB_AG,scala.collection.immutable.Map[AB_SI_ELM,Int],AB_TEMP_FORMULA)]()
      c_init_store = scala.collection.immutable.Map.empty ++ bb.storeContents

      println(" ")
      println("=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-")
      println(" ")
      println("           MODEL CHECKING STARTING                ")
      println("          LIMITED DEPTH-FIRST SEARCH              ")
      println(" ")
      println("=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-")
      println(" ")

      val mc_start_time = System.currentTimeMillis()

      print("ag = "); println(ag)
      print("ltl_form = "); println(ltl_form)

      val mc_res = real_check_lts(i,ag,ltl_form)
      val mc_end_time = System.currentTimeMillis()

      val mc_time_taken = mc_end_time - mc_start_time

      println(" "); println(" ")
      println("=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-")
      println(" ")
      print("MC has taken : "); print(mc_time_taken.toString); println(" ms")
      println(" ")      
      println("=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-")

      bb.set_store(c_init_store)

      mc_res
   }




   def real_check_lts(i: Int, ag: AB_AG, ltl_form: AB_TEMP_FORMULA):
                                           (Boolean,List[List[Int]],LPRIM) = {

      ltl_form match {

           case AB_LTL_STORE(f) => {
               if ( eval_ltl_store(f) ) { ( true, List[List[Int]](), List[AB_AST_Primitive]() )
	       } else { ( false, List[List[Int]](), List[AB_AST_Primitive]() )
	       }
           }

           case AB_LTL_NEXT(f) => {
	       val l_first_steps = lag_first_steps(ag)
	       var found = false
	       var clprim = List[AB_AST_Primitive]()
	       var result = (false,List[List[Int]](),List[AB_AG]())
	       var b = false
	       var lli = List[List[Int]]()
	       var lprim = List[AB_AST_Primitive]()

               c_store = scala.collection.immutable.Map.empty ++ bb.storeContents
               dfs_current_goals_bbs_ltls = (ag,c_store,ltl_form) :: dfs_current_goals_bbs_ltls
	       
               if (i == 0) {
	          result = ( false, List[List[Int]](), List[AB_AG]() )
	       } else {
                  result = ( false, List[List[Int]](), List[AB_AG]() )
                  for ( (lprim,cont,path_in_ag) <- l_first_steps ) {
                      if (!found) {
                          do_lprim(lprim)
			  c_gbf = (cont,scala.collection.immutable.Map.empty ++ bb.storeContents,f)
			  if (! c_gbf_included(c_gbf,dfs_current_goals_bbs_ltls)) {
                              real_check_lts(i-1,cont,f) match {
                                 case (b,lli,lprim_rec) => {
   			           if (b) {
                                     found = true
			             result = ( true, path_in_ag ::lli, lprim ::: lprim_rec )
                          } } } }
		          undo_lprim(lprim)
                      }
                  }
	       }

               dfs_current_goals_bbs_ltls = ab_remove_gbf(dfs_current_goals_bbs_ltls,
	                                        (ag,scala.collection.immutable.Map.empty ++ bb.storeContents,ltl_form)) 
	       result
	       
           }


           case AB_LTL_UNTIL(fi,fii) => {
               val (b_ii,lli_ii,lprim_ii) = real_check_lts(i,ag,fii)
	       if (b_ii) {
	         (b_ii,lli_ii,lprim_ii)
	       } else {
                 val (b_i,lli_i,lprim_i) = real_check_lts(i,ag,AB_LTL_STORE(fi))
		 if (b_i) {
                   real_check_lts(i,ag,AB_LTL_NEXT(AB_LTL_UNTIL(fi,fii)))
                 } else {
                   ( false, List[List[Int]](), List[AB_AST_Primitive]() )	   
                 }
	       }
           }

           case AB_LTL_REACH(f) => {
              c_store = scala.collection.immutable.Map.empty ++ bb.storeContents
              bfs_set_goals_bbs_sfs = Set[bfs_node]()
	      bfs_test_set_goals_bbs_sfs = Set( test_bfs_node(ag, f) )
              bfs_list_goals_bbs_sfs = List( bfs_node( mc_depth, ag, c_store, f, empty_node(), List[AB_AST_Primitive]() ))

              real_bfs_reach_check_lts()
           }

      }

  }

     def ab_remove_gbf(dfs_list:List[(AB_AG,scala.collection.immutable.Map[AB_SI_ELM,Int],AB_TEMP_FORMULA)],
                       e_gbf:(AB_AG,scala.collection.immutable.Map[AB_SI_ELM,Int],AB_TEMP_FORMULA)) = {
           dfs_list diff List(e_gbf)
     }

     def c_node_included_in_set(c_gbf: test_bfs_node,bfs_test_set_goals_bbs_sfs:Set[test_bfs_node]): Boolean = {
           bfs_test_set_goals_bbs_sfs(c_gbf)
     }


     def c_gbf_included(s:(AB_AG,scala.collection.immutable.Map[AB_SI_ELM,Int],AB_TEMP_FORMULA),
                        ls: List[(AB_AG,scala.collection.immutable.Map[AB_SI_ELM,Int],AB_TEMP_FORMULA)]): Boolean = {

          var b = false

          for (e <- ls) {
            if (!b) {
              (s,e) match {
	       case ( (a_i,bb_i,f_i), (a_ii,bb_ii,f_ii) ) => {
                 b = ( (a_i == a_ii) && (bb_i == bb_ii) && (f_i == f_ii) )
               }
            }
          } }

          b
	  
     } 


    def pp_bfs_set_goals_bbs_sfs() {
       for (e <- bfs_set_goals_bbs_sfs) {
         e match {
	   case bfs_node(i, ag, store, sf: AB_STORE_FORMULA, prev, lprim) => {
             print("  "); print(ag.ab_toString)
	     print(" <> "); println(sf.ab_toString)
           }
       }
     } }


     def pp_bfs_list_goals_bbs_sfs() {
       for (e <- bfs_list_goals_bbs_sfs) {
         e match {
	   case bfs_node(i, ag, store, sf: AB_STORE_FORMULA, prev, lprim) => {
             print("  "); print(ag.ab_toString)
	     print(" <> "); println(sf.ab_toString)
           }
       }
     } }

     def real_bfs_reach_check_lts(): (Boolean,List[List[Int]],LPRIM) = {

          var found_sol = false
          var found_error = false	  
          var first_ag = AB_AST_Empty_Agent()
	  var first_bb = scala.collection.immutable.Map[AB_SI_ELM,Int]()
	  var first_sf = STORE_TRUE_FORMULA()
          var rc_ans =   ( false, List[List[Int]](), List[AB_AG]() )

          var c_snode = bfs_node( 0, AB_AST_Empty_Agent(),
                                  scala.collection.immutable.Map[AB_SI_ELM,Int](),
	                          STORE_TRUE_FORMULA(),
	                          empty_node(),
	                          List[AB_AG]() )

          var test_c_snode = test_bfs_node( AB_AST_Empty_Agent(),
                                            STORE_TRUE_FORMULA() )


          var t_snode:search_node = bfs_node( 0, AB_AST_Empty_Agent(),
                                              scala.collection.immutable.Map[AB_SI_ELM,Int](),
	                                      STORE_TRUE_FORMULA(),
     	                                      empty_node(),
	                                      List[AB_AG]() )

          var tl_prim = List[AB_AG]()

          var jmjtxt_ii = " "

          var i = 0
	  
          while ( (! found_sol) && (! found_error) && (! (bfs_list_goals_bbs_sfs).isEmpty) ) {

              i = i+1
              print(". ")
	      if (i % 1000 == 0) { println(" "); println(i); println(" ") }
	  
              bfs_set_goals_bbs_sfs = bfs_set_goals_bbs_sfs + bfs_list_goals_bbs_sfs.head

              bfs_list_goals_bbs_sfs.head match {
                 case bfs_node(first_i,first_ag,first_bb,first_sf,_,_) => {
	            bb.set_store(first_bb)
                    if ( eval_ltl_store(first_sf) ) {
		         found_sol = true
			 t_snode = bfs_list_goals_bbs_sfs.head
                    } else { if (first_i > 0) {
                         for ( (lprim,cont,path_in_ag) <- lag_first_steps(first_ag) ) {
                            do_lprim(lprim)
                            c_snode = bfs_node(first_i-1,cont,scala.collection.immutable.Map.empty ++ bb.storeContents,first_sf,
			                       bfs_list_goals_bbs_sfs.head,lprim)
		            test_c_snode = test_bfs_node(c_snode.ag,c_snode.sf)
			    if (! c_node_included_in_set(test_c_snode,bfs_test_set_goals_bbs_sfs)) {
                                 bfs_list_goals_bbs_sfs = bfs_list_goals_bbs_sfs ::: List(c_snode)
    			         bfs_test_set_goals_bbs_sfs = bfs_test_set_goals_bbs_sfs + test_c_snode
		            }
		            undo_lprim(lprim)
                         }
                         bfs_list_goals_bbs_sfs = bfs_list_goals_bbs_sfs.tail }
                    }
                  }
                  case _ => {
                     found_error = true
		     println(" "); println("found error in MC"); println(" ")
                  }
              }
	  if (found_sol) {
	      while (t_snode != empty_node() ) {
	        t_snode match {
		   case bfs_node(_,_,_,_,prev,lprim) => {
                      tl_prim = lprim ::: tl_prim
		      t_snode = prev
		   }
		   case _ => { }
		}
              }
              rc_ans = ( true, List[List[Int]](), tl_prim )

          } else {
              rc_ans = ( false, List[List[Int]](), List[AB_AST_Primitive]() )		     
          }
	  }

	  
          rc_ans
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
       // To be implemented
       true
   }

   def exec_rule_primitive(prim:AB_Rule_Primitive):Boolean = {
       // To be implemented
       true
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


   def do_lprim(l: LPRIM) = {

       for (x <- l) {
         x match {
	   case AB_AST_Primitive(ab_prim,ab_prim_type) =>
                exec_primitive(ab_prim,ab_prim_type)
	   case _ =>
         } } 
	 
   }


   def exec_undo_primitive(prim:AB_Primitive,prim_type:AB_PRIMITIVE_TYPE):Boolean = {
        prim_type match {
            case AB_Prim_Type_Basic() => exec_undo_basic_primitive(prim.asInstanceOf[AB_Basic_Primitive])
            case AB_Prim_Type_Multi() => exec_undo_multi_primitive(prim.asInstanceOf[AB_Multi_Primitive])
            case AB_Prim_Type_Proc() => exec_undo_proc_primitive(prim.asInstanceOf[AB_Proc_Primitive])
            case AB_Prim_Type_Rule() => exec_undo_rule_primitive(prim.asInstanceOf[AB_Rule_Primitive])
            case AB_Prim_Type_Scene() => exec_undo_scene_primitive(prim.asInstanceOf[AB_Scene_Primitive])
         }
  }

   def exec_undo_basic_primitive(prim:AB_Basic_Primitive):Boolean = {
        prim match {
             case AB_Tell(stinfo) => bb.get(stinfo_simplify(stinfo))
             case AB_Ask(stinfo) => bb.ask(stinfo_simplify(stinfo))
             case AB_Nask(stinfo) => bb.nask(stinfo_simplify(stinfo))
             case AB_Get(stinfo) => bb.tell(stinfo_simplify(stinfo))
        }
   }

   def exec_undo_multi_primitive(prim:AB_Multi_Primitive):Boolean = {
        // To be implemented
        true
   }

   def exec_undo_proc_primitive(prim:AB_Proc_Primitive):Boolean = {
       // To be implemented
       true
   }

   def exec_undo_rule_primitive(prim:AB_Rule_Primitive):Boolean = {
       // To be implemented
       true
   }

   def exec_undo_scene_primitive(prim:AB_Scene_Primitive):Boolean = {
       // To be implemented
       true
    }


   def undo_lprim(l: LPRIM) = {

       for (x <- l) {
         x match {
	   case AB_AST_Primitive(prim,stinfo) =>
                exec_undo_primitive(prim,stinfo)
	   case _ =>
         } } 
	 

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

         case AB_AST_List_Primitive(lp) =>  AB_AST_List_Primitive(lp)
         case AB_Exec_AST_List_Primitive(prim,lprim,pp) =>  AB_AST_List_Primitive(prim::lprim)	 

         case AB_AST_List_Primitive(lp) =>  AB_AST_List_Primitive(lp)

         case AB_IF_THEN_Agent(c,ag) =>  AB_IF_THEN_Agent(c,run_unselect(ag))

         case AB_IF_THEN_ELSE_Agent(c,ag_i,ag_ii) =>  AB_IF_THEN_ELSE_Agent(c,run_unselect(ag_i),run_unselect(ag_ii)) 
       }
   }

   def exec_list_primitives(lprim:List[AB_AST_Primitive]) {

       for (p <- lprim) {
          p match {
	     case AB_AST_Primitive(pprim,pprim_type) => {
                exec_primitive(pprim,pprim_type) } }
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
              exec_list_primitives(prim::lprim)
	      AB_AST_Empty_Agent()
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
        // To be implemented
        true
   }

   def test_exec_rule_primitive(prim:AB_Rule_Primitive,bb:ABBlackboard):Boolean = {
        // To be implemented
        true
   }

   def test_exec_scene_primitive(prim:AB_Scene_Primitive,bb:ABBlackboard):Boolean = {
        // To be implemented
        true
   }
				  
}