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

import scala.collection.mutable.Map
import scala.swing._

class ABStore {

   var theStore = Map[AB_SI_ELM,Int]()

   def nb_occ(sit: AB_SI_ELM) = synchronized {
      if (theStore.contains(sit)) { theStore(sit) }
      else { 0 }
   }

   def storeContents = synchronized {
      theStore
   }


   def selectedStoreContents(l: List[AB_VAR_SI_ELM]) = synchronized {

      var theStoreRet = Map[AB_SI_ELM,Int]()
      for ((t,d) <- theStore) {
         if (matchFilters(t,l)) {
	   theStoreRet = theStoreRet ++ Map(t -> d)
	 }
      }
      theStoreRet

   }


   def set_store(s:scala.collection.immutable.Map[AB_SI_ELM,Int]) =
      synchronized {
          theStore = scala.collection.mutable.Map() ++ s
      }

   def tell(sit:AB_SI_ELM):Boolean = synchronized {
      if (theStore.contains(sit)) 
             { theStore(sit) = theStore(sit) + 1 }
      else
             { theStore = theStore ++ Map(sit -> 1) }
      true
   }

   def test_tell(sit:AB_SI_ELM):Boolean = true

   def ask(sit:AB_SI_ELM):Boolean = synchronized {   
      if ( theStore.contains(sit) ) 
             if (theStore(sit) >= 1) { true }
             else { false }
      else false
   }

   def test_ask(sit:AB_SI_ELM):Boolean = synchronized { 
      if (  theStore.contains(sit) ) 
             if (theStore(sit) >= 1) { true }
             else { false }
      else false
   }

   def get(sit:AB_SI_ELM):Boolean = synchronized {   
      if ( theStore.contains(sit) ) 
             if (theStore(sit) >= 1) 
               { theStore(sit) = theStore(sit) - 1
	         if (theStore(sit) == 0) { theStore = theStore - sit }
                 true 
               }
             else { false }
      else false
   }

   def test_get(sit:AB_SI_ELM):Boolean = test_ask(sit)

   def getall(sit:AB_SI_ELM):Boolean = {
      while (test_get(sit)) { get(sit) }
      true
   }


   def nask(sit:AB_SI_ELM):Boolean = synchronized {    
      if ( theStore.contains(sit) ) 
             if (theStore(sit) >= 1) { false }
             else { true }
      else 
             { true }

   }

   def test_nask(sit:AB_SI_ELM):Boolean = synchronized {    
      if ( theStore.contains(sit) ) 
             if (theStore(sit) >= 1) { false }
             else { true }
      else 
             { true }
   }

   def print_store {
      for ((t,d) <- theStore) 
         println ( t.ab_toString + "(" + theStore(t) + ")" )
   }

   def clear_store:Boolean = synchronized {
      theStore = Map[AB_SI_ELM,Int]()
      true
   }





}

