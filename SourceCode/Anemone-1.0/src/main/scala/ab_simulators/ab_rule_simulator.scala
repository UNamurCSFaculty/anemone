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
import scala.collection.mutable.Map

import scala.swing._
import javax.swing.SwingUtilities

class ABRuleSimulExec(var bb: ABBlackboard, var setScenes: ABSetOfScenes) {

   def rule_psi_verified(m: Map[AB_SI_ELM,Int]): Boolean = {
      var b = true
      for ((k,v) <- m) {
        if (b) { b = (bb.nb_occ(k) >= v) }
      }
      b
   }

   def rule_nsi_verified(l: List[AB_SI_ELM]): Boolean = {
      var b = true
      for (e <- l) {
        if (b) { b = (bb.nb_occ(e) == 0) }
      }
      b
   }

   def rule_ppc_verified(m: Map[String,Int]): Boolean = {
      var b = true
      for ((k,v) <- m) {
        if (b) { b = (bb.nb_occ_pc(k) >= v) }
      }
      b
   }

   def rule_npc_verified(l: List[String]): Boolean = {
      var b = true
      for (e <- l) {
        if (b) { b = (bb.nb_occ_pc(e) == 0) }
      }
      b
   }


   def rule_pre_verified(pre: List[AB_SIGNED_SI_PN_ELM], bb: ABBlackboard): Boolean = {

     val pSITerms = bb.posSITerms(pre)
     val nSITerms = bb.negSITerms(pre)
     val pPCalls = bb.posPCalls(pre)
     val nPCalls = bb.negPCalls(pre)     
     
     rule_psi_verified(pSITerms) && rule_nsi_verified(nSITerms) &&
     rule_ppc_verified(pPCalls) && rule_npc_verified(nPCalls)

   }

   def list_executable_rules = {

      val map_list_rule_exec = bb.list_rules_under_execution
      var l = List[AB_GEN_SPEC_RULE]()

      for ((k,v) <- map_list_rule_exec) {
         for (e <- v) {
            e match {
	       case  AB_SPEC_RULE(prName,pre,post) => {
                  if ( rule_pre_verified(pre,bb) ) {
                     l = AB_SPEC_RULE(prName,pre,post) :: l         
                  }
               }
	    }
	 }

     }
     l

   } 


   def rpost_exec_elm(e: AB_SIGNED_SI_PN_ELM) {
      e match {
        case AB_SSI_ELM(s:AB_SIGN,si_elm:AB_SI_ELM) => {
            s match {
               case ab_sign_plus() => { bb.tell(si_elm) }
	       case ab_sign_minus() => {
	          if (bb.test_get(si_elm)) { bb.get(si_elm)
		  } else { println("Error in rule execution, getting " + si_elm.ab_toString)
		  }
	       }
            }
         }
         case AB_SPC_ELM(s:AB_SIGN,pn:String,args:List[AB_SI_ELM]) => {
	    val proc_call = pn + "(" + args.map(e=>e.ab_toString).mkString(",") + ")"
            s match {
               case ab_sign_plus() => {
		 val ab_ag_pcall = AB_AST_Proc_Call(pn,args)
		 bb.tellp(proc_call,ab_ag_pcall,AB_INTERACTIVE_SIMUL(),setScenes) }
	       case ab_sign_minus() => {
	          if (bb.test_getp(proc_call)) { bb.getp(proc_call)
		  } else { println("Error in rule execution, getting proc " + proc_call) }
	       }
            }
         }
      }
   }


   def rpost_exec(post: List[AB_SIGNED_SI_PN_ELM]) {
      post match {
         case Nil => { }
	 case x::tail => {
           rpost_exec_elm(x)
	   rpost_exec(tail)
	 }
      }
   }

   def rule_post_exec(r:AB_SPEC_RULE) {
      r match {
         case AB_SPEC_RULE(prName, pre, post) => {
            rpost_exec(post)    
         }
      }
   }


   def exec_rule_selected(r: AB_SPEC_RULE) = rule_post_exec(r)

   def run_bb_rules {
         var random_gen = new Random()
         var lg = 0
         var rule_random_choice = 0
         var rules_currently_applicable = list_executable_rules	 

         SwingUtilities.invokeLater(new Runnable(){
              def run() {ab_window_agents.InteractiveBlackboard.redisplay_store} })
	 Thread.sleep(1000)
	      
         while (!rules_currently_applicable.isEmpty) {
	    lg = rules_currently_applicable.size
            rule_random_choice = random_gen.nextInt(lg)
            val rule_to_be_applied = rules_currently_applicable(rule_random_choice)
            exec_rule_selected(rule_to_be_applied.asInstanceOf[AB_SPEC_RULE])
            ab_window_agents.InteractiveBlackboard.redisplay_store
            rules_currently_applicable = list_executable_rules
            Thread.sleep(1000)
	 }
   }

} 