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

class InteractiveAgentTold(ag_label:String,ag:AB_AG,mybb: ABBlackboard, mySetScenes: ABSetOfScenes) extends Frame {

   val blue = new java.awt.Color(196, 226, 255)
   val green = new java.awt.Color(193, 255, 193)
   val red = new java.awt.Color(255, 176, 176)

   var agent_parsed:AB_AG= ag
   var res_agent:AB_AG = _
   var previous_agent:AB_AG = _
   var mySimulParser = new AnimBachSimulParser
   var myABsimul = new ABSimulExec(agent_parsed,mybb,mySetScenes)
   var current_agent:AB_AG = myABsimul.ag_first_steps(agent_parsed,List())

   myABsimul.simulAsType(AB_INTERACTIVE_SIMUL())


   class InteractiveStepButton(prim_txt:String,path:List[Int]) extends Button {
   
        this.text = prim_txt
        reactions += {
          case ButtonClicked(b) => execute_step(path) }

   }

   class abWidget
   case class AB_Label(txt_label: String) extends abWidget
   case class AB_Button(txt_ag: String,path:List[Int]) extends abWidget

   def translate(ab_ag: AB_AG) {

      def translate_into_widget_list(ag: AB_AG): List[abWidget] = {

         ag match {

            case AB_AST_Empty_Agent() => List( AB_Label("Empty agent") )
   
            case AB_AST_Primitive(prim,prim_type) =>  { 
                   val ag_label = prim.ab_toString
                   List( AB_Label(ag_label) ) }
   
            case AB_Exec_AST_Primitive(prim,prim_type,path) =>  {
                   val ag_label = prim.ab_toString
                   List( AB_Button(ag_label,path) ) }               
   
            case AB_AST_Proc_Call(pn,args) => {
                   val ag_label = ag.ab_toString
                   List( AB_Label(ag_label) ) }

            case AB_Exec_AST_Proc_Call(pn,args,path) => {
                   val ag_label = ag.ab_toString
                   List( AB_Button(ag_label,path) ) }
   
            case AB_AST_SEQ_Agent(ag_i,ag_ii) => { 
                   List( AB_Label( "[ " ) ) :::                         
                     translate_into_widget_list(ag_i) :::
                     List( AB_Label( " ] " + ";" + " [ " ) ) :::
                     translate_into_widget_list(ag_ii) :::
                     List( AB_Label( " ]" ) ) }                

            case AB_AST_PARA_Agent(ag_i,ag_ii) => { 
                   List( AB_Label( "[ " ) ) :::                         
                     translate_into_widget_list(ag_i) :::
                     List( AB_Label( " ] " + "||" + " [ " ) ) :::
                     translate_into_widget_list(ag_ii) :::
                     List( AB_Label( " ]" ) ) }                

            case AB_AST_CHOICE_Agent(ag_i,ag_ii) => { 
                   List( AB_Label( "[ " ) ) :::                         
                     translate_into_widget_list(ag_i) :::
                     List( AB_Label( " ] " + "+" + " [ " ) ) :::
                     translate_into_widget_list(ag_ii) :::
                     List( AB_Label( " ]" ) ) }                

            case AB_AST_List_Primitive(lag) => {
	           List( AB_Label( AB_AST_List_Primitive(lag).ab_toString )) }

            case AB_Exec_AST_List_Primitive(prim,lag,path) => {
	           val ag_label = prim.ab_toString
		   lag match {
                     case Nil => { List( AB_Button(ag_label,path) ) }
		     case _ => { 
                          List( AB_Button(ag_label,path) ) :::
			  List( AB_Label( "; " ) ) :::
		          translate_into_widget_list( AB_AST_List_Primitive(lag) ) }
	           } }

            case AB_IF_THEN_Agent(c,ag) => { 
                   List( AB_Label( "( " + c.ab_toString + " )" + "->" ) ) :::                         
                     translate_into_widget_list(ag) }

            case AB_IF_THEN_ELSE_Agent(c,ag_i,ag_ii) => { 
                   List( AB_Label( "( " + c.ab_toString + " )" + "->" ) ) :::                         
                     translate_into_widget_list(ag_i) :::
                     List( AB_Label( " <> " ) ) :::
                     translate_into_widget_list(ag_ii) }

        }

      }
      
      def group_ab_labels(txt_label:String,l_ab_wi:List[abWidget]): (String,List[abWidget]) = {
            
         l_ab_wi match {

            case List() => ( txt_label, l_ab_wi )

            case AB_Button(txt,path) :: l_res =>  ( txt_label, l_ab_wi )

            case AB_Label(txt) :: lres => group_ab_labels(txt_label+txt,lres)

         }

     }


      def group_ab_widgets(l_ab_wi: List[abWidget]) {

         l_ab_wi match {

            case List() => { }

            case AB_Button(txt_ag,path) :: l_res =>  {
                      theAgent.theCurrentAgentField.contents += newStepButton(txt_ag,path)
                      group_ab_widgets(l_res) }               

            case AB_Label(txt_label) :: lres => {
                      val (gen_label,ll_ab_wi) = group_ab_labels(txt_label,lres)
                      theAgent.theCurrentAgentField.contents += newAgTxt(gen_label) 
                      group_ab_widgets(ll_ab_wi) }

         }

      }

      def newAgTxt(txt_arg:String) = {
            new Label {  text = txt_arg
                         foreground = new java.awt.Color(0, 0, 0) 
                         background = red
                         opaque = true } 
      }

      def newStepButton(prim_txt: String,path:List[Int]) = {
           new InteractiveStepButton(prim_txt,path)
      }

      /*   to be actually executed in translate   */
      /*   ----------------------------------------   */

      group_ab_widgets(translate_into_widget_list(ab_ag))

   }

   def execute_step(path:List[Int]) {

      res_agent = myABsimul.run_selected(current_agent,path) 
      current_agent = myABsimul.ag_first_steps(res_agent,List())
      theAgent.theCurrentAgentField.contents.clear
      translate(current_agent)
      theAgent.theCurrentAgentField.revalidate()
      theAgent.theCurrentAgentField.repaint()
      abRealInterAgentPage.revalidate()
      abRealInterAgentPage.repaint()
      println("new agent : "+res_agent)
      InteractiveBlackboard.redisplay_store  

   }

   def refresh_agent {

      current_agent = myABsimul.ag_first_steps(res_agent,List())
      theAgent.theCurrentAgentField.contents.clear
      translate(current_agent)
      theAgent.theCurrentAgentField.revalidate()
      theAgent.theCurrentAgentField.repaint()

   }

/* ---------------------------------------------------------------------------------

   Window to introduce the agent to be processed

   Buttons are : 

      theSubmitAgentButton, to submit the agent ie to parse it into internal format
      theStepAgentButton, to run step by step the agent

---------------------------------------------------------------------------------- */

    val theAgent = new GridBagPanel {

       background = red

       /* Agent to be processed */
       /* --------------------  */

       val c = new Constraints
       val shouldFill = true
       if (shouldFill) { c.fill = Fill.Horizontal }

       val theAgentTitle = new Label { text = "Agent introduced in background" }
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
           background = red
           val theAbrevButton = new CheckBox("Abbrev")
	   val theEagerButton = new CheckBox("Eager")
	   opaque = true
	   contents += theAbrevButton
	   contents += theEagerButton
       }
       c.gridx = 2
       c.gridy = 0
       layout(theOptionBoxes) = c

       val theStrut_i = new Label { text = " " }
       c.gridx = 0
       c.gridy = 1
       layout(theStrut_i) = c

       val theAgentField = new TextArea(5, 60)
       theAgentField.text = ag_label
       val theScrollableAgentField = new ScrollPane(theAgentField)
       c.gridx = 1
       c.gridy = 1
       layout(theScrollableAgentField) = c



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


       val labelbb = new Label("currently empty")
       val theCurrentAgentField = new FlowPanel {
            background = red
            opaque = true
            contents += labelbb 
            hGap = 40
            vGap = 30
            border = Swing.EmptyBorder(15,10,10,10) }
       c.gridx = 1
       c.gridy = 3

       val theSCurrentAgentField = new ScrollPane(theCurrentAgentField)

       layout(theSCurrentAgentField) = c


       val theRefreshAgentButton = new Button { 
          text = "Refresh" }
       c.gridx = 2
       c.gridy = 3
       layout(theRefreshAgentButton) = c 

    }

/* --------------------------------------------------------------------------------

   Reactions to the buttons :

     from the main agent window : 
          theSubmitAgentButton, theRunAgentButton, theStepAgentButton
     from the history window : 
          thePAgentButton, theNAgentButton 

---------------------------------------------------------------------------------- */

     listenTo(theAgent.theRefreshAgentButton)
     reactions += {
         case ButtonClicked(theAgent.theRefreshAgentButton) => refresh_agent
 }

/* --------------------------------------------------------------

   Main elements

--------------------------------------------------------------- */

   val theStrutPanelI = new FlowPanel {
          background = red
          hGap = 40
          vGap = 20
          border = Swing.EmptyBorder(5,10,5,10) 
       }

   val theStrutPanelII = new FlowPanel {
          background = red
          hGap = 40
          vGap = 20
          border = Swing.EmptyBorder(5,10,5,10) 
       }

   val abRealInterAgentPage = new BoxPanel(Orientation.Vertical) {
         background = red
         opaque = true
         contents += theAgent
         border = Swing.EmptyBorder(30,30,10,10) }

   val abIAProcessesPage = new GridPanel(1, 1) {
        contents += new Label("Here will come the processes") {
        horizontalAlignment = Alignment.Center } }

   val abIATabbedPane = new TabbedPane {
        val theIAPage = new Page("Main agent",abRealInterAgentPage)
        val theIAProcessesPage = new Page("Processes",abIAProcessesPage)	   
        pages += theIAPage
        pages += theIAProcessesPage	   
  }

  theAgent.theCurrentAgentField.contents.clear
  translate(current_agent)
  theAgent.theCurrentAgentField.revalidate()
  theAgent.theCurrentAgentField.repaint()


   this.title = "Told interactive agent"
   this.visible = true



/* Alternative without tab                  */
/* ---------------------------------------- */

   this.contents = new BoxPanel(Orientation.Vertical) {
         background = red
         opaque = true
         contents += theAgent
         border = Swing.EmptyBorder(30,30,10,10) }


/* Alternative with tab                     */
/* ---------------------------------------- */

/*
   this.contents = new BorderPanel {
        layout(abIATabbedPane) = Center
   }

*/

   this.pack()
}



