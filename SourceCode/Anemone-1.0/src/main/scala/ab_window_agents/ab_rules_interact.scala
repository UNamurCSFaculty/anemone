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
import scala.util.Random


class InteractiveRuleAgent(mybb: ABBlackboard, mySetScenes: ABSetOfScenes) extends Frame {

   val blue = new java.awt.Color(196, 226, 255)
   val green = new java.awt.Color(193, 255, 193)
   val red = new java.awt.Color(255, 176, 176)
   val abYellow = new java.awt.Color(255, 255, 204)

   var myRSimul = new ABRuleSimulExec(mybb,mySetScenes)
   var rules_currently_applicable = List[AB_GEN_SPEC_RULE]()
   var rule_number = 0

   class InteractiveRuleButton(i: Int,rlabel: String) extends RadioButton {
      this.name = i.toString
      this.text = rlabel
      reactions += {
          case ButtonClicked(b) => { rule_number = i }
      }
   }

   var l_rule_radio_buttons = List[InteractiveRuleButton]()

   def list_rules_to_buttons(l:List[AB_GEN_SPEC_RULE]):List[InteractiveRuleButton] =  {
     var i = 0
     var r = List[InteractiveRuleButton]()
     for (e <- l) {
       r = new InteractiveRuleButton(i,e.ab_toString) :: r
       i = i+1
     }
     r
   }

   def init_rules {
      rules_currently_applicable = myRSimul.list_executable_rules
      l_rule_radio_buttons = list_rules_to_buttons(rules_currently_applicable)
   }

   def apply_selected_rule {
      val rule_to_be_applied = rules_currently_applicable(rule_number)
      myRSimul.exec_rule_selected(rule_to_be_applied.asInstanceOf[AB_SPEC_RULE])
      InteractiveBlackboard.redisplay_store
      rules_currently_applicable = myRSimul.list_executable_rules
      l_rule_radio_buttons = list_rules_to_buttons(rules_currently_applicable)
      theAgent.rule_selectors.contents.clear      
      theAgent.rule_selectors.contents ++= l_rule_radio_buttons 
      theAgent.rule_selectors.revalidate()
      theAgent.rule_selectors.repaint()	 
   }

   def ab_thread_run_rules {
     val ab_thread = new Thread {
            override def run { run_rules } 
     }
    ab_thread.start
   }

   def run_rules {
      var random_gen = new Random()
      var lg = 0
      var rule_random_choice = 0

      while (! l_rule_radio_buttons.isEmpty) {
         lg = l_rule_radio_buttons.size
         rule_random_choice = random_gen.nextInt(lg)
         val rule_to_be_applied = rules_currently_applicable(rule_random_choice)
         myRSimul.exec_rule_selected(rule_to_be_applied.asInstanceOf[AB_SPEC_RULE])
         InteractiveBlackboard.redisplay_store
         rules_currently_applicable = myRSimul.list_executable_rules
         l_rule_radio_buttons = list_rules_to_buttons(rules_currently_applicable)
         theAgent.rule_selectors.contents.clear      
         theAgent.rule_selectors.contents ++= l_rule_radio_buttons 
         theAgent.rule_selectors.revalidate()
         theAgent.rule_selectors.repaint()
         Thread.sleep(2000)	 
      }

     
   }

   def refresh_rules {
      rules_currently_applicable = myRSimul.list_executable_rules
      l_rule_radio_buttons = list_rules_to_buttons(rules_currently_applicable)
      theAgent.rule_selectors.contents.clear      
      theAgent.rule_selectors.contents ++= l_rule_radio_buttons 
      theAgent.rule_selectors.revalidate()
      theAgent.rule_selectors.repaint()	 
   }







/* ---------------------------------------------------------------------------------

   Window to introduce the rules to be processed

---------------------------------------------------------------------------------- */

    val dirFileSelector = List(
       new RadioButton() {
         name = "dir"
         text = "Directory"
       },
       new RadioButton() {
         name = "file"
         text = "File"
       }
     )

    val fruitSelector = List(
       new RadioButton() {
         name = "apple"
         text = "Apple"
       },
       new RadioButton() {
         name = "orange"
         text = "Orange"
       }
     )



      
    // new ButtonGroup(l_rule_radio_buttons: _*)


    val theAgent = new GridBagPanel {

       background = abYellow

       /* Rules to be processed */
       /* --------------------  */

       val c = new Constraints
       val shouldFill = true
       if (shouldFill) { c.fill = Fill.Horizontal }

       val theAgentTitle = new Label { text = "Please select a rule if any" }
       c.weightx = 0.5
       c.fill = Fill.None
       c.gridx = 0
       c.gridy = 0
       c.gridwidth = 2
       c.anchor = Anchor.West
       c.insets = new Insets(5,5,5,5)
       layout(theAgentTitle) = c

       val theRulesRefresh = new Button { text = "Refresh" }
       c.anchor = Anchor.East
       layout(theRulesRefresh) = c

       c.anchor = Anchor.West
       c.gridwidth = 1
       c.fill = Fill.Horizontal

       c.gridwidth = 1
       c.fill = Fill.Horizontal

       val theStrutI = new Label { text = " " }
       c.gridx = 0
       c.gridy = 1
       layout(theStrutI) = c

       // val rule_message = new Label { text = "No rule applicable" }
       // val rule_message_strut = new Label { text = " " }       

       init_rules
       val rule_selectors = new BoxPanel(Orientation.Vertical) {
              background = abYellow       
              contents ++= l_rule_radio_buttons 
       }
       val s_rules = new ScrollPane(rule_selectors) {
          preferredSize = new Dimension(600, 300)
	  border = Swing.EmptyBorder(15,10,10,10)
       }

       c.gridx = 0
       c.gridy = 2
       layout(s_rules) = c       

       val theStrutII = new Label { text = " " }
       c.gridx = 0
       c.gridy = 3
       layout(theStrutII) = c

       val theRulesAppButtons = new FlowPanel {

          val theApplySelectedRule = new Button { text = "Apply Selected Rule" }
          val theStrutCreateButtonI = new Label { text = "  " }
          val theRunRules = new Button { text = "Run Rules" }
          
          background = abYellow
          contents += theApplySelectedRule
          contents += theStrutCreateButtonI
          contents += theRunRules
       }

       c.gridx = 0
       c.gridy = 4
       layout(theRulesAppButtons) = c

    }


/* --------------------------------------------------------------------------------

   Reactions to the buttons :

---------------------------------------------------------------------------------- */

   listenTo(theAgent.theRulesAppButtons.theApplySelectedRule,
            theAgent.theRulesAppButtons.theRunRules,
	    theAgent.theRulesRefresh)
   reactions += {
     case ButtonClicked(theAgent.theRulesAppButtons.theApplySelectedRule) => apply_selected_rule
     case ButtonClicked(theAgent.theRulesAppButtons.theRunRules) => ab_thread_run_rules
     case ButtonClicked(theAgent.theRulesRefresh) => refresh_rules
   }




/* --------------------------------------------------------------

   Main elements

--------------------------------------------------------------- */

   this.title = "Rules currently applicable"
   this.visible = true

   this.contents = new BoxPanel(Orientation.Vertical) {
         background = abYellow
         opaque = true
         contents += theAgent
         border = Swing.EmptyBorder(30,30,10,10) }

   this.pack()

}
