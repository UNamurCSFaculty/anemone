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


package ab_window_agents

import ab_data._
import ab_parser._
import ab_blackboard._
import ab_scene._
import ab_simulators._

import scala.swing._
import scala.swing.event._
import GridBagPanel._
import java.awt.Insets
import java.awt.Color
import scala.collection.mutable.Map


class InteractiveProcedureAgent(ag_id:Int,mybb: ABBlackboard,mySetScenes: ABSetOfScenes) extends Frame {

   val blue = new java.awt.Color(196, 226, 255)
   val green = new java.awt.Color(193, 255, 193)
   val red = new java.awt.Color(255, 176, 176)
   val pink = new java.awt.Color(255, 204, 255)   

   var dfn_to_be_parsed = new String
   var ack_user: String = ""   

   var mySetsTranslator = new PrettyPrinterSetDefns
   var mySceneTranslator = new PrettyPrinterSceneDefns
   var myProcsTranslator = new PrettyPrinterProcsDefns      
   var myEqnsTranslator = new PrettyPrinterEqnsDefns
   var myRulesTranslator = new PrettyPrinterRulesDefns   
   
   var mySimulParser = new AnimBachSimulParser

   def newAgTxt(txt_arg:String) = {
            new Label {  text = txt_arg
                         foreground = new java.awt.Color(0, 0, 0) 
                         background = pink
                         opaque = true } 
      }

   def parse_description {
      dfn_to_be_parsed = theAgent.theAgentField.text

      try {
         var description_parsed = mySimulParser.parse_prgm(dfn_to_be_parsed)
         var aboptset = description_parsed._1
         var aboptmap = description_parsed._2   
         var abopteqn = description_parsed._3
         var aboptscene = description_parsed._4
         var aboptproc = description_parsed._5
	 var aboptrule = description_parsed._6

         if (aboptset != None) {
            val real_sets = aboptset.asInstanceOf[List[AB_SET]]
            ack_user = ack_user + "set(s) introduced " + "\n" + mySetsTranslator.translate(real_sets)
            mybb.addDecls(real_sets)
         }


         if (abopteqn != None) {
            val real_eqns = abopteqn.asInstanceOf[List[AB_MAP_EQ]]      
            ack_user = ack_user + "eqns(s) introduced " + "\n" + myEqnsTranslator.translate(real_eqns)
            mybb.addEqns(real_eqns)
         }

         if (aboptscene != None) {
            val real_scene = aboptscene.asInstanceOf[List[AB_SCENE_DEFN]]            
            ack_user = ack_user + "scene element(s) introduced " + "\n" + mySceneTranslator.translate(real_scene)
   	    mySetScenes.addSceneDecls(real_scene)
         }

         if (aboptproc != None) {
            val real_procs = aboptproc.asInstanceOf[List[AB_Abs_Proc]]
            ack_user = ack_user + "proc(s) introduced " + "\n" + myProcsTranslator.translate(real_procs)
            mybb.addProcDecls(real_procs)	 
         }

         if (aboptrule != None) {
            val real_rules = aboptrule.asInstanceOf[List[AB_GEN_RULE]]
            ack_user = ack_user + "rule(s) introduced " + "\n" + myRulesTranslator.translate(real_rules)
            mybb.addRuleDecls(real_rules)	 
         }

         theAgent.theCurrentAgentField.text = ack_user

      } catch {
          case e: Exception => {
             theAgent.theCurrentAgentField.text = "/!\\ Parsing error /!\\ " + e
             println("exception caught: " + e)
	  }
      }

   }

   def parse_proc_dfn = parse_description
   

/* ---------------------------------------------------------------------------------

   Window to introduce the agent to be processed

   Button is : 

      theSubmitAgentButton, to submit the agent ie to parse it into internal format

---------------------------------------------------------------------------------- */

    val theAgent = new GridBagPanel {

       background = pink

       /* Agent to be processed */
       /* --------------------  */

       val c = new Constraints
       val shouldFill = true
       if (shouldFill) { c.fill = Fill.Horizontal }

       val theAgentTitle = new Label { text = "Set/map/scene/proc description(s) to be processed" }
       c.weightx = 0.5
       c.fill = Fill.None
       c.gridx = 0
       c.gridy = 0
       c.gridwidth = 3
       c.anchor = Anchor.West
       c.insets = new Insets(5,5,5,5)
       layout(theAgentTitle) = c

       c.gridwidth = 1
       c.fill = Fill.Horizontal

       val theStrut_i = new Label { text = " " }
       c.gridx = 0
       c.gridy = 1
       layout(theStrut_i) = c

       val theAgentField = new TextArea(10, 60)
       theAgentField.text = "Enter the description(s)"
       val theScrollableAgentField = new ScrollPane(theAgentField)
       c.gridx = 1
       c.gridy = 1
       layout(theScrollableAgentField) = c

       val theSubmitAgentButton = new Button { 
          text = "Submit" }
       c.gridx = 2
       c.gridy = 1
       layout(theSubmitAgentButton) = c 



       /* Current value of the agent */
       /* -------------------------  */

       val theCurrentAgentTitle = new Label { text = "Description(s) parsed" }
       c.weightx = 0.5
       c.fill = Fill.None
       c.gridx = 0
       c.gridy = 2
       c.gridwidth = 3
       c.anchor = Anchor.West
       c.insets = new Insets(5,5,5,5)
       layout(theCurrentAgentTitle) = c

       c.gridwidth = 1
       c.fill = Fill.Horizontal

       val theStrut_ii = new Label { text = " " }
       c.gridx = 0
       c.gridy = 3
       layout(theStrut_ii) = c


       val theCurrentAgentField = new TextArea(10,60)
       theCurrentAgentField.text = "Here will be displayed the description(s)"
       theCurrentAgentField.editable = false
       theCurrentAgentField.lineWrap = true       
       val theScrollableCurrentAgentField = new ScrollPane(theCurrentAgentField)
       c.gridx = 1
       c.gridy = 4
       layout(theScrollableCurrentAgentField) = c

       border = Swing.EmptyBorder(15,10,15,10) 

    }

/* --------------------------------------------------------------------------------

   Reactions to the buttons :

     from the main agent window : 
          theSubmitAgentButton, theRunAgentButton, theStepAgentButton
     from the history window : 
          thePAgentButton, theNAgentButton 

---------------------------------------------------------------------------------- */

     listenTo(theAgent.theSubmitAgentButton)
	      
     reactions += {
         case ButtonClicked(theAgent.theSubmitAgentButton) => parse_proc_dfn
     }

/* --------------------------------------------------------------

   Main elements

--------------------------------------------------------------- */

   val theStrutPanelI = new FlowPanel {
          background = pink
          hGap = 40
          vGap = 20
          border = Swing.EmptyBorder(5,10,5,10) 
       }

   val theStrutPanelII = new FlowPanel {
          background = pink
          hGap = 40
          vGap = 20
          border = Swing.EmptyBorder(5,10,5,10) 
       }

   this.title = "Agent number " + ag_id.toString
   this.visible = true
    this.contents = new BoxPanel(Orientation.Vertical) {
         background = pink
         opaque = true
         contents += theAgent
         border = Swing.EmptyBorder(30,30,10,10) }
   this.pack()
}



