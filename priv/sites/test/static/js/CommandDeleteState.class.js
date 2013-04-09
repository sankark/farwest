/**
	This class inherit from draw2d.Command. Its funtion is to support undo/redo mechanism.
	@params parent and child are objects inherited from GenericModel class
	@class
*/
CommandDeleteState=function(figure){
    this.page = figure.getModel().page;
	this.figure = figure;

	this.commandDelete = new draw2d.CommandDelete(figure);
}

CommandDeleteState.prototype = new draw2d.Command;

/** @private **/
CommandDeleteState.prototype.type="CommandDeleteState";


/** Execute the command the first time */
CommandDeleteState.prototype.execute=function(){
   this.redo();
}


/** Undo the command */
CommandDeleteState.prototype.redo=function(){

	if ( this.page != null ){
		var state = this.figure.getModel();
		this.page.removeChild( state );
		state.page = null;
	}

	this.commandDelete.redo();
	
	//fix title bug
	this.figure.setTitle( this.figure.getModel().name );
}


/** Redo the command after the user has undo this command */
CommandDeleteState.prototype.undo=function(){

	if ( this.page != null ){
		var state = this.figure.getModel();
		state.page = this.page;
		this.page.addChild( state );
	}

	this.commandDelete.undo();
	
	//fix title bug
	this.figure.setTitle( this.figure.getModel().name );

}
