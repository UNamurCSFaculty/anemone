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


class InteractiveModelCheckerAgent(ag_id:Int,mybb: ABBlackboard, mySetScenes: ABSetOfScenes) extends Frame {

   val blue = new java.awt.Color(196, 226, 255)
   val green = new java.awt.Color(193, 255, 193)
   val red = new java.awt.Color(255, 176, 176)
   val yellow = new java.awt.Color(255, 229, 204)
   
   var agent_to_be_parsed = new String
   var agent_parsed:AB_AG= _
   var formula_to_be_parsed = new String
   var formula_parsed:AB_TEMP_FORMULA= _
   var mySimulParser = new AnimBachSimulParser
   var myABsimulCheck = new ABSimulCheckLTS(mybb,mySetScenes)
   var myFormulaTranslator = new PrettyPrinterFormula
   var depthMChecker = 15 // 100
   var mc_b = false
   var mc_path = List[List[Int]]()
   var mc_lprim = List[AB_AG]()
 
   def parse_agent {
      agent_to_be_parsed = theAgent.theAgentField.text

      try { 
         agent_parsed = mySimulParser.parse_agent(agent_to_be_parsed)
         theAgent.theCurrentAgentField.text = agent_parsed.ab_toString
      } catch {
         case e: Exception => {
            theAgent.theCurrentAgentField.text = "/!\\ Parsing error /!\\ " + e
            println("exception caught: " + e)
         }
      }

   } 

   def parse_formula {
      formula_to_be_parsed = theAgent.theFormulaField.text

      try {
         formula_parsed = mySimulParser.parse_formula(formula_to_be_parsed)
         theAgent.theCurrentFormulaField.text = myFormulaTranslator.translate(formula_parsed)
      } catch {
         case e: Exception => {
            theAgent.theCurrentAgentField.text = "/!\\ Parsing error /!\\ " + e
            println("exception caught: " + e)
         }
      }
      
   }

   def model_check_formula:Boolean = {
      myABsimulCheck.check_lts(depthMChecker,agent_parsed,formula_parsed) match {
         case (b,path,lprim) => { mc_b = b; mc_path = path; mc_lprim = lprim }
      }
      if (mc_b) {
         theAgent.theMCStatusTitle.text = "Formula established"
      } else {
         theAgent.theMCStatusTitle.text = "Formula not established"
      }
      mc_b
   }


   def st_execute_step(p:AB_AG) = {

     p match {
        case AB_AST_Primitive(prim,prim_type) =>
           myABsimulCheck.exec_primitive(prim,prim_type)

        case _ =>
     }
      
   }

   def st_execute_all = {

       for (x <- mc_lprim) {
          st_execute_step(x)
	  println(x.ab_toString)
          InteractiveBlackboard.redisplay_store  	  
	  Thread.sleep(2000)

       }

   }
   
   def st_ab_thread_execute_all {
     val st_ab_thread = new Thread {
            override def run { st_execute_all } 
     }
    st_ab_thread.start
   }
    

   def simulate_trace { st_ab_thread_execute_all }



/* ---------------------------------------------------------------------------------

   Window to introduce the agent to be processed

   Buttons are : 

      theSubmitAgentButton, to submit the agent ie to parse it into internal format
      theStepAgentButton, to run step by step the agent

---------------------------------------------------------------------------------- */

    val theAgent = new GridBagPanel {

       background = yellow

       /* Agent to be processed */
       /* --------------------  */

       val c = new Constraints
       val shouldFill = true
       if (shouldFill) { c.fill = Fill.Horizontal }

       val theAgentTitle = new Label { text = "Agent to be processed" }
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
       theAgentField.text = "Enter the agent"
       val theScrollableAgentField = new ScrollPane(theAgentField)
       c.gridx = 1
       c.gridy = 1
       layout(theScrollableAgentField) = c

       val theSubmitAgentButton = new Button { 
          text = "Submit" }
       c.gridx = 2
       c.gridy = 1
       layout(theSubmitAgentButton) = c 


       /* Agent parsed */
       /* -------------------------  */

       val theCurrentAgentTitle = new Label { text = "Agent introduced" }
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

       val theCurrentAgentField = new Label  {
          text = "Here will be displayed the parsed agent" }
       c.gridx = 1
       c.gridy = 3
       layout(theCurrentAgentField) = c

       val theStrut_iii = new Label { text = " " }
       c.gridx = 0
       c.gridy = 4
       layout(theStrut_iii) = c


       /*  Formula to be parsed  */
       /*  --------------------  */

       val theFormulaTitle = new Label { text = "Formula to be processed" }
       c.weightx = 0.5
       c.fill = Fill.None
       c.gridx = 0
       c.gridy = 5
       c.gridwidth = 3
       c.anchor = Anchor.West
       c.insets = new Insets(5,5,5,5)
       layout(theFormulaTitle) = c

       c.gridwidth = 1
       c.fill = Fill.Horizontal

       val theStrut_iv = new Label { text = " " }
       c.gridx = 0
       c.gridy = 6
       layout(theStrut_iv) = c

       val theFormulaField = new TextArea(10, 60)
       theFormulaField.text = "Enter the formula"
       val theScrollableFormulaField = new ScrollPane(theFormulaField)
       c.gridx = 1
       c.gridy = 6
       layout(theScrollableFormulaField) = c

       val theSubmitFormulaButton = new Button { 
          text = "Submit" }
       c.gridx = 2
       c.gridy = 6
       layout(theSubmitFormulaButton) = c 


       /* Formula parsed */
       /* -------------  */

       val theCurrentFormulaTitle = new Label { text = "Formula introduced" }
       c.weightx = 0.5
       c.fill = Fill.None
       c.gridx = 0
       c.gridy = 7
       c.gridwidth = 3
       c.anchor = Anchor.West
       c.insets = new Insets(5,5,5,5)
       layout(theCurrentFormulaTitle) = c

       c.gridwidth = 1
       c.fill = Fill.Horizontal

       val theStrut_vii = new Label { text = " " }
       c.gridx = 0
       c.gridy = 8
       layout(theStrut_vii) = c

       val theCurrentFormulaField = new Label  {
          text = "Here will be displayed the parsed formula" }
       c.gridx = 1
       c.gridy = 8
       layout(theCurrentFormulaField) = c


       /* Formula parsed */
       /* -------------  */

       val theStrut_viii = new Label { text = " " }
       c.gridx = 0
       c.gridy = 9
       layout(theStrut_viii) = c

       val theStrut_ix = new Label { text = " " }
       c.gridx = 0
       c.gridy = 10
       layout(theStrut_ix) = c

       c.gridwidth = 2	  
       val theModelCheckButton = new Button { 
          text = "Model Check" }
       c.gridx = 1
       c.gridy = 10
       layout(theModelCheckButton) = c 

       c.gridwidth = 1
       val theStrut_ixx = new Label { text = " " }
       c.gridx = 0
       c.gridy = 11
       layout(theStrut_ixx) = c


       c.gridwidth = 2	  
       val theMCStatusTitle = new Label { text = "Status of the model checking" }
       c.fill = Fill.None
       c.gridx = 1
       c.gridy = 11
       layout(theMCStatusTitle) = c

       c.gridwidth = 1
       val theStrut_x = new Label { text = " " }
       c.gridx = 0
       c.gridy = 12
       layout(theStrut_x) = c

       c.gridwidth = 2
       val theSimulateTraceButton = new Button { 
          text = "Simulate trace" }
       c.gridx = 1
       c.gridy = 12
       layout(theSimulateTraceButton) = c 

    }

/* --------------------------------------------------------------------------------

   Reactions to the buttons :

     from the main agent window : 
          theSubmitAgentButton, theRunAgentButton, theStepAgentButton
     from the history window : 
          thePAgentButton, theNAgentButton 

---------------------------------------------------------------------------------- */

     listenTo(theAgent.theSubmitAgentButton,
              theAgent.theSubmitFormulaButton,
              theAgent.theModelCheckButton,
	      theAgent.theSimulateTraceButton)
     reactions += {
         case ButtonClicked(theAgent.theSubmitAgentButton) => parse_agent
         case ButtonClicked(theAgent.theSubmitFormulaButton) => parse_formula
         case ButtonClicked(theAgent.theModelCheckButton) => model_check_formula
         case ButtonClicked(theAgent.theSimulateTraceButton) => simulate_trace	 
 }

/* --------------------------------------------------------------

   Main elements

--------------------------------------------------------------- */

   val theStrutPanelI = new FlowPanel {
          background = yellow
          hGap = 40
          vGap = 20
          border = Swing.EmptyBorder(5,10,5,10) 
       }

   val theStrutPanelII = new FlowPanel {
          background = yellow
          hGap = 40
          vGap = 20
          border = Swing.EmptyBorder(5,10,5,10) 
       }

   this.title = "Agent number " + ag_id.toString
   this.visible = true
    this.contents = new BoxPanel(Orientation.Vertical) {
         background = yellow
         opaque = true
         contents += theAgent
         border = Swing.EmptyBorder(30,30,10,10) }
   this.pack()
}



