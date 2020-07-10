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
import scala.collection.mutable.ArrayBuffer


object InteractiveBlackboard extends SimpleSwingApplication {

   UIManager.setLookAndFeel(new NimbusLookAndFeel)

   val blue = new java.awt.Color(219, 242, 255)
   val green = new java.awt.Color(176, 255, 226)
   val red = new java.awt.Color(255, 176, 176)
   val pink = new java.awt.Color(255, 204, 255)
   
   var mybb = new ABBlackboard
   var mySetScenes = new ABSetOfScenes
   var mySimulParser = new AnimBachSimulParser

   var myListFilters = List[AB_VAR_SI_ELM]()
   var filtersOn = false

   def clear_store {

      mybb.clear_store
      store_to_label(mybb.storeContents) 
      println("Cleared the store")

   }


   def redisplay_store {

      store_to_label(mybb.storeContents)
      mySetScenes.redraw

   }


   def selected_bb_contents = {

     if (filtersOn) {
        mybb.selectedStoreContents(myListFilters)
     } else {
        mybb.storeContents
     }

   }


   def tell_on_store {

      val token_arg = theCurrentStore.theStoreButtons.theSTokenField.text
      try {
         val sit = mySimulParser.parse_stinfo(token_arg)
         try {
	    val multiplicity_arg = (theCurrentStore.theStoreButtons.theSDensityField.text).toInt
            for (i <- 1 to multiplicity_arg) { mybb.tell(sit) }
            store_to_label(selected_bb_contents)  
            mybb.print_store 
            theCurrentStore.warningMessage.text = "No warning"	    
	 } catch {
             case e: Exception => {
                theCurrentStore.warningMessage.text = "/!\\ Please enter integer /!\\ " + e
                println("exception caught: " + e)
	     }
         }
      } catch {
         case e: Exception => {
            theCurrentStore.warningMessage.text = "/!\\ Please enter a correct si-term /!\\ " + e
            println("exception caught: " + e)
         }
      }

   }

   
   def get_from_store {

      val token_arg = theCurrentStore.theStoreButtons.theSTokenField.text
      try {      
         val sit = mySimulParser.parse_stinfo(token_arg)      
         try {
	    val multiplicity_arg = (theCurrentStore.theStoreButtons.theSDensityField.text).toInt
            for (i <- 1 to multiplicity_arg) { mybb.get(sit) }
            store_to_label(selected_bb_contents) 
	 } catch {
             case e: Exception => {
                theCurrentStore.warningMessage.text = "/!\\ Please enter integer /!\\ " + e
                println("exception caught: " + e)
	     }
         }
      } catch {
         case e: Exception => {
            theCurrentStore.warningMessage.text = "/!\\ Please enter a correct si-term /!\\ " + e
            println("exception caught: " + e)
         }
      }

   }

   def filter_the_store {

      val theFiltersField = theCurrentStore.theFilterButtons.theFilterSearchField.text
      try {      
         val lf = mySimulParser.parse_var_stinfo(theFiltersField)      
         myListFilters = lf
	 filtersOn = true
         store_to_label(selected_bb_contents)
         theCurrentStore.warningMessage.text = "No warning"	    
      } catch {
         case e: Exception => {
            theCurrentStore.warningMessage.text = "/!\\ Error in list of filters /!\\ " + e
            println("exception caught: " + e)
         }
      }

   }


   def unfilter_the_store {

      filtersOn = false
      store_to_label(selected_bb_contents)
      theCurrentStore.warningMessage.text = "No warning"	    

   }
   

   def store_to_label(theStore:Map[AB_SI_ELM,Int]) {

      def newToken(sti:AB_SI_ELM,nb:Int) = { 
            new Label {  text = sti.ab_toString + " [" + nb.toString + "]" + "  "
                         foreground = new java.awt.Color(0, 0, 0) 
                         background = blue
                         opaque = true } 
      }

      theCurrentStore.bbObj.contents.clear
      for ((t,d) <- theStore) 
         { theCurrentStore.bbObj.contents += newToken(t,d) }
      theCurrentStore.bbObj.revalidate()
      theCurrentStore.bbObj.repaint()	 

   }


   var nb_agent = 0

   def c_n_auto_agent {

      nb_agent = nb_agent + 1
      val new_auto_agent = new InteractiveAutoAgent(nb_agent,mybb,mySetScenes)
   }


   def c_n_inter_agent {

      nb_agent = nb_agent + 1
      val new_inter_agent = new InteractiveInterAgent(nb_agent,mybb,mySetScenes)
   }


   def c_n_proc_agent {

      nb_agent = nb_agent + 1
      val new_inter_agent = new InteractiveProcedureAgent(nb_agent,mybb,mySetScenes)
   }

   def c_n_model_checker_agent {

      nb_agent = nb_agent + 1
      val new_inter_agent = new InteractiveModelCheckerAgent(nb_agent,mybb,mySetScenes)
   }

   def c_n_rule_agent {

      nb_agent = nb_agent + 1
      val new_inter_agent = new InteractiveRuleAgent(mybb,mySetScenes)
   }


/* ---------------------------------------------------------------------------------

   Window to introduce the store

   Buttons are : 

      theStoreClearButton, to clear the store
      theTellButton, to add a specified token with a specified density
      theGetButton, density to be treated
      theFilterButton, set list of declared var-si-terms to filter the
                       displayed contents of the store
      theUnfilterButton, set to unfilter
      
---------------------------------------------------------------------------------- */


    val theCurrentStore = new GridBagPanel {

       background = blue

       val c = new Constraints
       val shouldFill = true
       if (shouldFill) { c.fill = Fill.None }
       
       val theStoreTitle = new Label { text = "Current store" }
       c.weightx = 0.5
       c.fill = Fill.None
       c.gridx = 0
       c.gridy = 0
       c.gridwidth = 2
       c.anchor = Anchor.West
       c.insets = new Insets(5,5,5,5)
       layout(theStoreTitle) = c

       val theStoreClearButton = new Button { text = "Clear" }
       c.anchor = Anchor.East
       layout(theStoreClearButton) = c

       c.anchor = Anchor.West
       c.gridwidth = 1
       c.fill = Fill.Horizontal

       val theStrutI = new Label { text = " " }
       c.gridx = 0
       c.gridy = 1
       layout(theStrutI) = c

       val labelbb = new Label("currently empty")
       val bbObj = new FlowPanel {
            background = blue
            opaque = true
            contents += labelbb 
            hGap = 40
            vGap = 30
            border = Swing.EmptyBorder(15,10,10,10) }
       c.weightx=0.5
       c.weighty=1
       c.gridx = 1
       c.gridy = 1
       val SbbObj = new ScrollPane(bbObj)
       layout(SbbObj) = c

       val theStrutII = new Label { text = " " }
       c.gridx = 0
       c.gridy = 2
       layout(theStrutII) = c

       val theStoreButtons = new FlowPanel {
          background = blue
          val theTellButton = new Button { text = "Tell" }
          val theGetButton = new Button { text = "Get" }
          val theSTokenText = new Label { text = "si-term: " }
          val theSTokenField = new TextField { columns = 15 
                                               text = "t" }
          val theSDensityText = new Label { text = "multiplicity: " }
          val theSDensityField = new TextField { columns = 10 
                                                 text = "1" }
          opaque = true
          contents += theTellButton
          contents += theSTokenText
          contents += theSTokenField
          contents += theSDensityText
          contents += theSDensityField
          contents += theGetButton
          hGap = 40
          vGap = 20
          border = Swing.EmptyBorder(5,10,5,10) 
       }

       c.gridx = 1
       c.gridy = 2
       layout(theStoreButtons) = c 

       val theStrutIIbis = new Label { text = " " }
       c.gridx = 0
       c.gridy = 3
       layout(theStrutIIbis) = c

       val theFilterButtons = new FlowPanel {
          background = blue
          val theFilterButton = new Button { text = "Filter" }
          val theUnfilterButton = new Button { text = "Unfilter" }
          val theFilterSearchField = new TextField { columns = 40 
                                               text = "t(?X,3), u(9)" }
          opaque = true
          contents += theFilterButton
          contents += theFilterSearchField
          contents += theUnfilterButton
          hGap = 40
	  vGap = 0
          border = Swing.EmptyBorder(5,10,5,10) 
       }

       c.gridx = 1
       c.gridy = 3
       layout(theFilterButtons) = c

       val strutWarningMessage = new Label {text = " " }
       c.gridx = 1
       c.gridy = 4
       layout(strutWarningMessage) = c

       val warningMessage = new Label {text = "No warning" }
       c.gridx = 1
       c.gridy = 5
       layout(warningMessage) = c

       border = Swing.EmptyBorder(15,10,15,10) 

    }






/* ---------------------------------------------------------------------------------

   Window to create agents
   Buttons are : 
      theCreateAgentButton to create a new agent process

---------------------------------------------------------------------------------- */


    val theCreateAgentButtons = new FlowPanel {

       val theCreateAutoAgentButton = new Button { text = "New Autonomous Agent" }
       val theStrutCreateButtonI = new Label { text = "  " }
       val theCreateInterAgentButton = new Button { text = "New Interactive Agent" }
       val theStrutCreateButtonII = new Label { text = "  " }
       val theCreateInterProcButton = new Button { text = "New Definition(s)" }
       val theStrutCreateButtonIII = new Label { text = "  " }
       val theCreateInterMCButton = new Button { text = "New Model Checker" }
       val theStrutCreateButtonIV = new Label { text = "  " }
       val theCreateInterRuleButton = new Button { text = "New Rule Checker" }
       

       background = blue
       contents += theCreateInterAgentButton
       contents += theStrutCreateButtonI
       contents += theCreateAutoAgentButton
       contents += theStrutCreateButtonII
       contents += theCreateInterProcButton
       contents += theStrutCreateButtonIII
       contents += theCreateInterMCButton
       contents += theStrutCreateButtonIV
       contents += theCreateInterRuleButton
    }



/* --------------------------------------------------------------------------

   Main Window

--------------------------------------------------------------------------- */    

   val theStrutPanelI = new FlowPanel {
          background = blue
          hGap = 40
          vGap = 20
          border = Swing.EmptyBorder(5,10,5,10) 
       }

   val theStrutPanelAgButtons = new FlowPanel {
          background = blue
          hGap = 40
          vGap = 10
          border = Swing.EmptyBorder(5,5,5,5) 
       }


/*     For a non-tab version         */
/* --------------------------------  */

   def top = new MainFrame {
      title = "The interactive blackboard"
      contents = new BoxPanel(Orientation.Vertical) {
         background = blue
         opaque = true
         contents += theCurrentStore
         contents += theStrutPanelI
         contents += theCreateAgentButtons
	 contents += theStrutPanelAgButtons
         border = Swing.EmptyBorder(30,30,10,10) }
      }


     val abBBContentPage = new BoxPanel(Orientation.Vertical) {
             background = blue
             opaque = true
             contents += theCurrentStore
             contents += theStrutPanelI
             contents += theCreateAgentButtons
             border = Swing.EmptyBorder(30,30,30,30) }

     val abBBRulesPage = new GridPanel(1, 1) {
                                contents += new Label("Here will come the rules") {
                                     horizontalAlignment = Alignment.Center } }

     val abBBProcessesPage = new GridPanel(1, 1) {
                                contents += new Label("Here will come the processes") {
                                     horizontalAlignment = Alignment.Center } }

     val abTabbedPane = new TabbedPane {
           val theBBPage = new Page("Contents",abBBContentPage)
	   val theRulesPage = new Page("Rules",abBBRulesPage)
	   val theProcessesPage = new Page("Processes",abBBProcessesPage)	   
	   pages += theBBPage
	   pages += theRulesPage
	   pages += theProcessesPage	   
     }


/*    for a tab version    */
/* ----------------------- */

/*
    def top = new MainFrame {
         title = "The interactive blackboard"
         contents = new BorderPanel {
            layout(abTabbedPane) = Center
           }
    }

*/


/* --------------------------------------------------------------------------------

   Reactions to the buttons :

     from the current store window (theCurrentStore) :
          theStoreClearButton, theTellButton, theGetButton,
	  theFilterButton, the UnfilterButton
     from the agent window : 
          theCreateAgentButton

---------------------------------------------------------------------------------- */


   listenTo( theCurrentStore.theStoreClearButton,
             theCurrentStore.theStoreButtons.theTellButton,
             theCurrentStore.theStoreButtons.theGetButton,
             theCurrentStore.theFilterButtons.theFilterButton,
             theCurrentStore.theFilterButtons.theUnfilterButton,
             theCreateAgentButtons.theCreateAutoAgentButton,
             theCreateAgentButtons.theCreateInterAgentButton,
	     theCreateAgentButtons.theCreateInterProcButton,
     	     theCreateAgentButtons.theCreateInterMCButton,
	     theCreateAgentButtons.theCreateInterRuleButton)
   reactions += {
     case ButtonClicked(theCurrentStore.theStoreClearButton) => clear_store
     case ButtonClicked(theCurrentStore.theStoreButtons.theTellButton) => tell_on_store
     case ButtonClicked(theCurrentStore.theStoreButtons.theGetButton) => get_from_store
     case ButtonClicked(theCurrentStore.theFilterButtons.theFilterButton) => filter_the_store
     case ButtonClicked(theCurrentStore.theFilterButtons.theUnfilterButton) => unfilter_the_store
     case ButtonClicked(theCreateAgentButtons.theCreateInterAgentButton) => c_n_inter_agent
     case ButtonClicked(theCreateAgentButtons.theCreateAutoAgentButton) => c_n_auto_agent
     case ButtonClicked(theCreateAgentButtons.theCreateInterProcButton) => c_n_proc_agent
     case ButtonClicked(theCreateAgentButtons.theCreateInterMCButton) => c_n_model_checker_agent
     case ButtonClicked(theCreateAgentButtons.theCreateInterRuleButton) => c_n_rule_agent }     

}



