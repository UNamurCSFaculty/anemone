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


class ABSets {

   var theSetDecls = Map[String,AB_SET]()
   var elmsInSets = Map[String,List[(String,Int)]]()   

   def addSetDecl(setDecl: AB_SET) = synchronized {
     setDecl match {
        case AB_SET_DEF(setname,setelms) =>
           if (!(theSetDecls.contains(setname))) {
	      theSetDecls += (setname -> setDecl)
	      var i = 0
	      for (e<-setelms) {
	        if (elmsInSets.contains(e)) {
		   elmsInSets(e) = (setname,i) :: elmsInSets(e)
                } else {
	           elmsInSets = elmsInSets ++ Map(e->List((setname,i)))
		}
		i = i+1
	      }
           }
        }
   }
   
   def addDecls(ld : List[AB_SET]) = synchronized {
      for (e<-ld) { addSetDecl(e) }
   }

   def findDecl(setName:String):AB_SET = synchronized {
      AB_SET_DEF("to be defined",List("to be defined"))
   }

   def setMembers(setName:String) = synchronized {
      if (theSetDecls.contains(setName)) {
        theSetDecls(setName) match {
	   case AB_SET_DEF(sn,selms) => selms
	   case _ => List[String]()
	}
      } else {
        List[String]()
      }
   }

   def ordDecl(setElm: String) = {
      if (elmsInSets.contains(setElm)) {
        elmsInSets(setElm) match
	{ case Nil => -1
	  case (setname,idelm) :: l => idelm
	}
      } else { -1
      }
   }



}