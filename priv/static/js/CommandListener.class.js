/**
	This class is mainly intended to support additional operation when a figure is added or remove (directly or because of redo/undo) 
	Tnherit draw2d.CommandStackEventListener.
	@class
*/
CommandListener=function( /*:draw2d.workflow*/ workflow ){
	draw2d.CommandStackEventListener.call(this);
	this.workflow = workflow;
}
CommandListener.prototype = new  draw2d.CommandStackEventListener;


/**
	Sent when an event occurs on the command stack. draw2d.CommandStackEvent.getDetail() 
	can be used to identify the type of event which has occurred.
	@private
*/
CommandListener.prototype.stackChanged=function(/*:draw2d.CommandStackEvent*/ event){
	
	
	//get the command details
	var details = event.getDetails();
	var command = event.getCommand();
	var event_type = null;
	var event_step = event_step = (event.isPostChangeEvent()) ? "POST" : "PRE";
	
	if (0 != (details & (draw2d.CommandStack.PRE_EXECUTE | draw2d.CommandStack.POST_EXECUTE)))
		event_type = "EXECUTE";
	else if (0 != (details & (draw2d.CommandStack.PRE_UNDO | draw2d.CommandStack.POST_UNDO)))
		event_type = "UNDO";
	else if (0 != (details & (draw2d.CommandStack.PRE_REDO | draw2d.CommandStack.POST_REDO)))
		event_type = "REDO";
	else
		event_type = null;

		

	
	/** Function @private */
	var changePageOfState = function( command, event_type ){
		var state = ( command.figure.getModel && (command.figure.getModel() instanceof State) ) ? command.figure.getModel() : null;
		var old_page = (command.oldCompartment!=null && command.oldCompartment.getModel && (command.oldCompartment.getModel() instanceof Page) ) ? command.oldCompartment.getModel() : null;
		var new_page = (command.newCompartment!=null && command.newCompartment.getModel && (command.newCompartment.getModel() instanceof Page) ) ? command.newCompartment.getModel() : null;

		if (event_type == 'UNDO'){
			temp = old_page;
			old_page = new_page;
			new_page = temp;
		}

		if ( state == null )
			return;
			
		if ( state.page != new_page ){
			state.page = new_page;
			
			if (old_page!=null)
				old_page.removeChild( state );
			
			if (new_page!=null)
				new_page.addChild( state );

			propertyDialog.refresh();
		}	
	}
	
	
	/** Function @private */
	var reconnectTransition = function( command,event_type ){
		var source = command.con.sourcePort.parentNode.getModel();
		var target = command.con.targetPort.parentNode.getModel();
		
		command.con.getModel().source = source;
		command.con.getModel().target = target;
		
		command.con.setRouter( new TransitionRouter(command.con.sourcePort , command.con.targetPort, command.con) );
	}

	
	//filter the command by its parent class
	if (event_step=='POST'){
		if(command instanceof draw2d.CommandAdd){}
		else if(command instanceof draw2d.CommandDelete){}
		else if(command instanceof draw2d.CommandMove){
			changePageOfState( command,event_type );
		}
		else if(command instanceof draw2d.CommandReconnect){
			reconnectTransition( command );
		}
		else if(command instanceof draw2d.CommandResize){}
		else if(command instanceof draw2d.CommandConnect){ }
		else if(command instanceof CommandChangeModelProperty){ }
		else if(command instanceof CommandAddModelChild){ }
		else if(command instanceof CommandRemoveModelChild){ }
		else{}	
		
		

		if ( command instanceof draw2d.CommandDelete | command instanceof CommandDeletePage | command instanceof CommandDeleteState )
			this.workflow.setCurrentSelection(null);
		else{
			propertyDialog.refresh();
		}
		
		if (!(command instanceof draw2d.CommandResize))
			elementsListDialog.refresh();
		
		
		this.workflow.setDocumentDirty();
	}
	
}
