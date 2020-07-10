/***********************************************************************

 Copyright (c) 2020 Jean-Marie Jacquet and the CoordiNam Lab members
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

package ab_blackboard

import ab_data._
import ab_parser._
import ab_scene._
import ab_window_agents.InteractiveAgentTold
import ab_window_agents.AutoAgentTold

import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer
import scala.swing._

class ABProcs(bb:ABBlackboard) {


   /* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
   
                       data       

   =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= */

   var theProcDecls = Map[String,(List[(String,String)],AB_AG)]()
   var theProcStore = Map[String,ArrayBuffer[Frame]]()



   /* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
   
                 proc definitions       

   =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= */

   def addProcDecl(d: AB_Abs_Proc) {
      d match {
        case AB_Proc_Def(pn,pargs,ag) => 
           if (theProcDecls.contains(pn)) {
	     theProcDecls(pn) = (pargs,ag)
	   } else {
	     theProcDecls = theProcDecls ++ Map(pn->(pargs,ag))
	   }
     }
   }
   
   def addProcDecls(ld : List[AB_Abs_Proc]) {
      for (d <- ld) { addProcDecl(d) }
   }

   def procFormalArgs(pn:String):List[AB_SI_ELM] = {
      if (theProcDecls.contains(pn)) {
        val fStrArgs = theProcDecls(pn)._1
	fStrArgs.map(e => {e match { case (a,b) => AB_SI_ATOMIC(a) }})
      } else { 
        List[AB_SI_ELM]()
      }
   }

   def procBody(pn:String):AB_AG = {
      if (theProcDecls.contains(pn)) {
        theProcDecls(pn)._2
      } else { 
        AB_AST_Empty_Agent()
      }
   }

   def procDefined(pn:String):Boolean = {
      theProcDecls.contains(pn)
   }


   /* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
   
       Handling procedure calls as active data       

   =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= */

   def ab_Add_Proc_Agent(proc_call_str: String, new_window_ref: Frame) {
      if (theProcStore.contains(proc_call_str)) {
           theProcStore(proc_call_str) += new_window_ref
      } else {
           theProcStore = theProcStore ++ Map(proc_call_str->ArrayBuffer(new_window_ref))
      }
   }

   def nb_occ_pc(proc_call_str:String): Int = {
      if (theProcStore.contains(proc_call_str)) {
            theProcStore(proc_call_str).size
      } else {
            0
      }
   }

   def tellp(proc_call_str:String,proc_call_ast:AB_AG,simulType:AB_SIMUL_TYPE,mySetScenes:ABSetOfScenes): Boolean = {
     simulType match {
        case AB_INTERACTIVE_SIMUL() => {
           val new_inter_told_agent = new InteractiveAgentTold(proc_call_str,proc_call_ast,bb,mySetScenes)
	   ab_Add_Proc_Agent(proc_call_str,new_inter_told_agent) }
        case AB_AUTONOMOUS_SIMUL() => {
           val new_auto_told_agent = new AutoAgentTold(proc_call_str,proc_call_ast,bb,mySetScenes)
	   ab_Add_Proc_Agent(proc_call_str,new_auto_told_agent) }	   
     }
     true 
   }

   def test_tellp(proc_call_str:String,proc_call_ast:AB_AG,simulType:AB_SIMUL_TYPE,mySetScenes:ABSetOfScenes): Boolean = {
     true
   }

   def getp(proc_call_str:String): Boolean = {
      if (theProcStore.contains(proc_call_str)) {
         if (theProcStore(proc_call_str).size != 0) {
            val first_window = theProcStore(proc_call_str).head
	    val l_win_rem = theProcStore(proc_call_str).tail
	    theProcStore(proc_call_str) = theProcStore(proc_call_str).tail
            first_window.dispose()	  
	    true
	 } else {
	    false
         }
      } else {
          false
      }
   }

   def test_getp(proc_call_str:String): Boolean = {
      if (theProcStore.contains(proc_call_str)) {
         if (theProcStore(proc_call_str).size != 0) { true
	 } else { false
         }
      } else { false
      }
   }

   def getallp(proc_call_str:String): Boolean = {
      while (test_getp(proc_call_str)) { getp(proc_call_str) }
      true
   }
   
   def askp(proc_call_str:String): Boolean = {
      if (theProcStore.contains(proc_call_str)) {
         if (theProcStore(proc_call_str).size != 0) { true
	 } else { false }
      } else {
          false
      }
   }

   def test_askp(proc_call_str:String): Boolean = {
      if (theProcStore.contains(proc_call_str)) {
         if (theProcStore(proc_call_str).size != 0) { true
	 } else { false }
      } else { false
      }
   }

   def naskp(proc_call_str:String): Boolean = {
      if (theProcStore.contains(proc_call_str)) {
         if (theProcStore(proc_call_str).size != 0) { false
	 } else { true }
      } else {
          true
      }
   }

   def test_naskp(proc_call_str:String): Boolean = {
      if (theProcStore.contains(proc_call_str)) {
         if (theProcStore(proc_call_str).size != 0) { false
	 } else { true }
      } else { true
      }
   }

}