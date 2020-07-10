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

package ab_blackboard

import ab_data._
import ab_parser._
import ab_scene._
import ab_window_agents.InteractiveAgentTold
import ab_window_agents.AutoAgentTold


import scala.collection.mutable.Map
import scala.swing._


class ABBlackboard {


   /* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
   
                 data declarations

   =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= */

   val theStore = new ABStore
   val theSets = new ABSets
   val theEqns = new ABEqns
   var theProcs = new ABProcs(this)
   var theRules = new ABRules



   /* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
   
                 Store functions

   =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= */


   def tell(sit:AB_SI_ELM):Boolean = theStore.tell(sit)
   def test_tell(sit:AB_SI_ELM):Boolean = theStore.test_tell(sit)

   def ask(sit:AB_SI_ELM):Boolean = theStore.ask(sit)
   def test_ask(sit:AB_SI_ELM):Boolean = theStore.test_ask(sit)
   
   def get(sit:AB_SI_ELM):Boolean = theStore.get(sit)
   def test_get(sit:AB_SI_ELM):Boolean = theStore.test_ask(sit)
   def getall(sit:AB_SI_ELM):Boolean = theStore.getall(sit)

   def nask(sit:AB_SI_ELM):Boolean = theStore.nask(sit)
   def test_nask(sit:AB_SI_ELM):Boolean = theStore.test_nask(sit)

   def nb_occ(sit: AB_SI_ELM) = theStore.nb_occ(sit)
   def storeContents = theStore.storeContents
   def selectedStoreContents(l: List[AB_VAR_SI_ELM]) =
          theStore.selectedStoreContents(l)
   def set_store(s:scala.collection.immutable.Map[AB_SI_ELM,Int]) =
          theStore.set_store(s)
   def print_store = theStore.print_store
   def clear_store:Boolean = theStore.clear_store



   /* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
   
                 Set functions

   =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= */

   def addSetDecl(setDecl: AB_SET) = theSets.addSetDecl(setDecl)
   def addDecls(ld : List[AB_SET]) = theSets.addDecls(ld)
   def findDecl(setName:String):AB_SET = theSets.findDecl(setName)
   def ordDecl(setElm: String) = theSets.ordDecl(setElm)
   def setMembers(setName:String) = theSets.setMembers(setName)



   /* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
   
                 Eqns functions

   =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= */

   def addEqns(leqns : List[AB_MAP_EQ]) = theEqns.addEqns(leqns)
   def listEqns: List[AB_MAP_EQ] = theEqns.listEqns
   
   //   To be completed




   /* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
   
                 Proc functions       

   =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= */

   def addProcDecl(d: AB_Abs_Proc) = theProcs.addProcDecl(d)
   def addProcDecls(ld : List[AB_Abs_Proc]) = theProcs.addProcDecls(ld)
   def procFormalArgs(pn:String):List[AB_SI_ELM] = theProcs.procFormalArgs(pn)
   def procBody(pn:String):AB_AG = theProcs.procBody(pn)
   def procDefined(pn:String):Boolean = theProcs.procDefined(pn)

   def tellp(proc_call_str:String,proc_call_ast:AB_AG,simulType:AB_SIMUL_TYPE,mySetScenes:ABSetOfScenes): Boolean =
     theProcs.tellp(proc_call_str,proc_call_ast,simulType,mySetScenes)
   def test_tellp(proc_call_str:String,proc_call_ast:AB_AG,simulType:AB_SIMUL_TYPE,mySetScenes:ABSetOfScenes): Boolean =
     theProcs.test_tellp(proc_call_str,proc_call_ast,simulType,mySetScenes)

   def getp(proc_call_str:String): Boolean = theProcs.getp(proc_call_str)
   def test_getp(proc_call_str:String): Boolean = theProcs.test_getp(proc_call_str)
   def getallp(proc_call_str:String): Boolean = theProcs.getallp(proc_call_str)
   
   def askp(proc_call_str:String): Boolean = theProcs.askp(proc_call_str)
   def test_askp(proc_call_str:String): Boolean = theProcs.test_askp(proc_call_str)

   def naskp(proc_call_str:String): Boolean = theProcs.naskp(proc_call_str)
   def test_naskp(proc_call_str:String): Boolean = theProcs.test_naskp(proc_call_str)

   def nb_occ_pc(proc_call_str:String): Int = theProcs.nb_occ_pc(proc_call_str)


   /* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
   
                 Rule functions       

   =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= */

   def addRuleDecls(d: List[AB_GEN_RULE]) = theRules.addRuleDecls(d)

   def tellr(ruleName:String) = theRules.tellr(ruleName,this)
   def askr(ruleName:String) = theRules.askr(ruleName)
   def getr(ruleName:String) = theRules.getr(ruleName)
   def naskr(ruleName:String) = theRules.naskr(ruleName)

   def test_tellr(ruleName:String) = theRules.test_tellr(ruleName)
   def test_askr(ruleName:String) = theRules.test_askr(ruleName)
   def test_getr(ruleName:String) = theRules.test_getr(ruleName)
   def test_naskr(ruleName:String) = theRules.test_naskr(ruleName)

   def list_rules_under_execution = theRules.list_rules_under_execution
   def ab_existRuleUE = theRules.ab_existRuleUE

   def posSITerms(pre: List[AB_SIGNED_SI_PN_ELM]) = theRules.posSITerms(pre)
   def negSITerms(pre: List[AB_SIGNED_SI_PN_ELM]) = theRules.negSITerms(pre)
   def posPCalls(pre: List[AB_SIGNED_SI_PN_ELM]) = theRules.posPCalls(pre)
   def negPCalls(pre: List[AB_SIGNED_SI_PN_ELM]) = theRules.negPCalls(pre)

}