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
     Map definitions
     ---------------
*/ 


class AB_MAP
case class AB_MAP_SGN(mapname: String, setsgn: AB_SET_EXPR) extends AB_MAP



/*
     Map equations
     -------------
*/

abstract class AB_MAP_EQN_ELM {
   def ab_toString: String = { " " }
   def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_MAP_EQN_ELM
}

case class AB_MAP_ATOMIC_ELM(name: String) extends AB_MAP_EQN_ELM {
   override def ab_toString: String = { name }
   override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_MAP_EQN_ELM = {
      val str_xs = xs.map(x=>{ x match { case AB_SI_ATOMIC(f) => f }})
      val str_ts = xs.map(x=>x.ab_toString)
      if (str_xs.contains(name)) {
         val i = str_xs.indexOf(name)
	 AB_MAP_ATOMIC_ELM(str_ts(i))
       } else { AB_MAP_ATOMIC_ELM(name)
       }
   }
}

case class AB_MAP_COMPOSED_ELM(functorid:String, functorargs: List[AB_MAP_EQN_ELM]) extends AB_MAP_EQN_ELM {
   override def ab_toString: String = { functorid + "(" + (functorargs.map(x=>x.ab_toString)).mkString(",") + ")" }
   override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_MAP_EQN_ELM = {
      AB_MAP_COMPOSED_ELM(functorid, functorargs.map(x=>x.ab_substitute(xs,ts)))
   }
}


class AB_MAP_DEFN
case class AB_MAP_EQ(elmdefined:AB_SI_ELM, elmdefinition : AB_SI_ELM) extends AB_MAP_DEFN

