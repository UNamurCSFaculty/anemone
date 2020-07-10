package object ab_data {


   /*
         General functions for si-terms
         ------------------------------
   */


   def matchAFilter(t: AB_SI_ELM, f: AB_VAR_SI_ELM): Boolean = {
   
      f match {
        case AB_VAR_ELM(x) => { true }
        case AB_VAR_SI_TERM(s) => { s == t }
        case AB_VAR_SI_COMPOSED(f_functor,f_functorArgs) => {
           t match {
             case AB_SI_COMPOSED(t_functor,t_functorArgs) => {
   	       (f_functor == t_functor) && listMatchAFilter(t_functorArgs,f_functorArgs) }
   	     case _ => { false }
           }
        }
      }
   
   }

   def listMatchAFilter(lt: List[AB_SI_ELM], lf: List[AB_VAR_SI_ELM]): Boolean = {
   
      if (lt.length == lf.length) {
        listEqLMatchAFilter(lt, lf)
      } else {
        false
      }
      
   }
   
   def listEqLMatchAFilter(lt: List[AB_SI_ELM], lf: List[AB_VAR_SI_ELM]): Boolean = {
   
      lt match {
        case Nil => { true }
        case t::lt_rem => {
           lf match {
             case Nil => false
    	     case f::lf_rem => {
   	        if (matchAFilter(t,f)) { listEqLMatchAFilter(lt_rem,lf_rem)
   	        } else { false }
   	     }
           }
        }
      }
   
   }

   def matchFilters(t: AB_SI_ELM, lf: List[AB_VAR_SI_ELM]): Boolean = {
   
      lf match {
        case Nil => { false }
        case f::lf_rem => {
           if (matchAFilter(t,f)) { true 
           } else { matchFilters(t,lf_rem)
   	}
        }
      }
   
   }

   def si_toInt(t: AB_SI_ELM, d: Int):Int = {
     try { (t.ab_toString).toInt
     } catch {
       case e:Throwable => d
     }
   }
   
   def si_toFloat(t: AB_SI_ELM, d: Float):Float = {
     try { (t.ab_toString).toFloat
     } catch {
       case e:Throwable => d
     }
   }

   def si_toDouble(t: AB_SI_ELM, d: Double):Double = {
     try { (t.ab_toString).toDouble
     } catch {
       case e:Throwable => d
     }
   }



}