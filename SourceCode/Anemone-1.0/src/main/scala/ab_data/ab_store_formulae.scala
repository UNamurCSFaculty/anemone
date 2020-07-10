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
     Store formulae
     --------------
*/


abstract class AB_STORE_FORMULA_EXPRESSION {
   def ab_toString: String = { " " }
}
case class AB_STR_F_INT(i:Int) extends AB_STORE_FORMULA_EXPRESSION {
   override def ab_toString: String = { i.toString }
}
case class AB_STR_F_SINFO(s:AB_SI_ELM) extends AB_STORE_FORMULA_EXPRESSION {
   override def ab_toString: String = { "#" + s.ab_toString }
}



abstract class AB_STORE_FORMULA {
   def ab_toString: String = { " " }
}
case class STORE_TRUE_FORMULA() extends AB_STORE_FORMULA {
   override def ab_toString: String = { "true" }
}
case class STORE_FALSE_FORMULA() extends AB_STORE_FORMULA {
   override def ab_toString: String = { "false" }
}
case class STORE_ATOMIC_FORMULA(relop: String,argi:AB_STORE_FORMULA_EXPRESSION,
                                argii:AB_STORE_FORMULA_EXPRESSION) extends AB_STORE_FORMULA {
   override def ab_toString: String = { argi.ab_toString + relop + argii.ab_toString }
}
case class STORE_COMPI_FORMULA(boolop: String,arg:AB_STORE_FORMULA) extends AB_STORE_FORMULA {
   override def ab_toString: String = { boolop + "( " + arg.ab_toString + " )" }
}
case class STORE_COMPII_FORMULA(boolop: String,argi:AB_STORE_FORMULA,argii:AB_STORE_FORMULA) extends AB_STORE_FORMULA {
   override def ab_toString: String = { "( " + argi.ab_toString + " )"  + boolop + "( " + argii.ab_toString + " )" }
}


abstract class AB_TEMP_FORMULA {
    def ab_toString: String = { " " }
 }

case class AB_LTL_STORE(f: AB_STORE_FORMULA) extends AB_TEMP_FORMULA {
    override def ab_toString: String = { f.ab_toString }
}
case class AB_LTL_NEXT(f:AB_TEMP_FORMULA) extends AB_TEMP_FORMULA {
    override def ab_toString: String = { "Next ( " + f.ab_toString + " )" }

}
case class AB_LTL_UNTIL(fi:AB_STORE_FORMULA,fii:AB_TEMP_FORMULA) extends AB_TEMP_FORMULA {
    override def ab_toString: String = { "( " + fi.ab_toString + " )" + " Until ( " + fii.ab_toString + " )" }
}
case class AB_LTL_REACH(f:AB_STORE_FORMULA) extends AB_TEMP_FORMULA {
    override def ab_toString: String = { "Reach ( " + f.ab_toString + " )" }

}


