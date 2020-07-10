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

import scala.swing.GridBagPanel._
import scala.swing.BorderPanel.Position._
import scala.swing.TabbedPane._

import javax.swing.SwingUtilities
import javax.swing.UIManager
import javax.swing.plaf.nimbus.NimbusLookAndFeel

import java.awt.Insets
import java.awt.Color

import scala.collection.mutable.Map

class InteractiveAutoAgent(ag_id:Int,mybb: ABBlackboard, mySetScenes: ABSetOfScenes) extends Frame {

   val blue = new java.awt.Color(196, 226, 255)
   val green = new java.awt.Color(193, 255, 193)
   val red = new java.awt.Color(255, 176, 176)

   var agent_to_be_parsed = new String
   var agent_parsed:AB_AG = _
   var current_agent:AB_AG = _
   var previous_agent:AB_AG = _
   var mySimulParser = new AnimBachSimulParser
   var myABsimul = new ABSimulExec(agent_parsed,mybb,mySetScenes)
   var myRSimul = new ABRuleSimulExec(mybb,mySetScenes)

   myABsimul.simulAsType(AB_AUTONOMOUS_SIMUL())

   def parse_agent {
      agent_to_be_parsed = theAgent.theAgentField.text

      try { 
         agent_parsed = mySimulParser.parse_agent(agent_to_be_parsed)
         current_agent = agent_parsed
         theAgent.theCurrentAgentField.text = current_agent.ab_toString
      } catch {
          case e: Exception => {
             theAgent.theCurrentAgentField.text = "/!\\ Parsing error /!\\ " + e
             current_agent = AB_AST_Empty_Agent()
             println("exception caught: " + e)
	  }
      }
   }
 
   def execute_step:Boolean = {
       var exec_result = true
       previous_agent = current_agent
       if (current_agent != AB_AST_Empty_Agent()) {
          exec_result = myABsimul.run_one(current_agent) match 
               { case (false,_)          => false
                 case (true,new_agent)  => 
                    { current_agent = new_agent
		      if (mybb.ab_existRuleUE) {
			Thread.sleep(1000)
                        myRSimul.run_bb_rules
                      }
                      true
                    }
               }
          theAgent.theCurrentAgentField.text = current_agent.ab_toString
          SwingUtilities.invokeLater(new Runnable(){
              def run() {InteractiveBlackboard.redisplay_store} })
       }
       exec_result
   }

   def execute_all = {

       var failure = false
//       var failure_aux = false  // for eager evaluation
       while ( current_agent != AB_AST_Empty_Agent() && !failure ) {
//          failure_aux = !execute_step  // for eager evaluation
          failure = !execute_step
	  Thread.sleep(2000)

       }

   }
   
   def ab_thread_execute_all {
     val ab_thread = new Thread {
            override def run { execute_all } 
     }
    ab_thread.start
   }



/* ---------------------------------------------------------------------------------

   Window to introduce the agent to be processed

   Buttons are : 

      theSubmitAgentButton, to submit the agent ie to parse it into internal format
      theRunAgentButton, to run the agent from begin to end
      theStepAgentButton, to run step by step the agent

---------------------------------------------------------------------------------- */

    val theAgent = new GridBagPanel {

       background = green

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
       c.gridwidth = 2
       c.anchor = Anchor.West
       c.insets = new Insets(5,5,5,5)
       layout(theAgentTitle) = c

       c.gridwidth = 1
       c.fill = Fill.Horizontal

       val theOptionBoxes = new FlowPanel {
           background = green
           val theAbrevButton = new CheckBox("Abbrev")
	   val theEagerButton = new CheckBox("Eager")
	   opaque = true
	   contents += theAbrevButton
//	   contents += theEagerButton
       }
       c.gridx = 2
       c.gridy = 0
       layout(theOptionBoxes) = c

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

       /* Current value of the agent */
       /* -------------------------  */

       val theCurrentAgentTitle = new Label { text = "Current agent" }
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
          text = "Here will be displayed the current agent" }
       c.gridx = 1
       c.gridy = 3
       layout(theCurrentAgentField) = c

       /* The running buttons */
       /* ------------------- */

       val theRunningButtons = new FlowPanel {
          background = green
          val theRunAgentButton = new Button { text = "Run" }
          val theStepAgentButton = new Button { text = "Next" }
          opaque = true
          contents += theRunAgentButton
          contents += theStepAgentButton
          hGap = 40
          vGap = 20
          border = Swing.EmptyBorder(5,10,5,10) 
       }

       c.gridx = 0
       c.gridy = 4
       c.gridwidth = 3
       layout(theRunningButtons) = c 

       border = Swing.EmptyBorder(15,10,15,10) 

    }

/* --------------------------------------------------------------------------------

   Reactions to the buttons :

     from the main agent window : 
          theSubmitAgentButton, theRunAgentButton, theStepAgentButton
     from the history window : 
          thePAgentButton, theNAgentButton 

---------------------------------------------------------------------------------- */

   listenTo(theAgent.theSubmitAgentButton,
            theAgent.theRunningButtons.theStepAgentButton,
            theAgent.theRunningButtons.theRunAgentButton)
   reactions += {
     case ButtonClicked(theAgent.theSubmitAgentButton) => parse_agent
     case ButtonClicked(theAgent.theRunningButtons.theStepAgentButton) => execute_step
     case ButtonClicked(theAgent.theRunningButtons.theRunAgentButton) => ab_thread_execute_all
   }

/* --------------------------------------------------------------

   Main elements

--------------------------------------------------------------- */

   val theStrutPanelI = new FlowPanel {
          background = green
          hGap = 40
          vGap = 20
          border = Swing.EmptyBorder(5,10,5,10) 
       }

   val theStrutPanelII = new FlowPanel {
          background = green
          hGap = 40
          vGap = 20
          border = Swing.EmptyBorder(5,10,5,10) 
       }

   val abRealAutoAgentPage = new BoxPanel(Orientation.Vertical) {
         background = green
         opaque = true
         contents += theAgent
         border = Swing.EmptyBorder(30,30,10,10) }

   val abAAProcessesPage = new GridPanel(1, 1) {
        contents += new Label("Here will come the processes") {
        horizontalAlignment = Alignment.Center } }

   val abAATabbedPane = new TabbedPane {
        val theAAPage = new Page("Main agent",abRealAutoAgentPage)
        val theAAProcessesPage = new Page("Processes",abAAProcessesPage)	   
        pages += theAAPage
        pages += theAAProcessesPage	   
  }

   this.title = "Autonomous agent number " + ag_id.toString
   this.visible = true

/*   for a non-tab version  */
/* ------------------------ */

   this.contents = new BoxPanel(Orientation.Vertical) {
         background = green
         opaque = true
         contents += theAgent
         border = Swing.EmptyBorder(30,30,10,10) }

/*  for a tabbed version  */
/*  --------------------  */

/*
   this.contents = new BorderPanel {
        layout(abAATabbedPane) = Center
   }
*/

   this.pack()

}
