/**
	This class inherit from draw2d.Command. Its funtion is to support undo/redo mechanism.
	@class
*/

CommandDeletePage=function(figure){

	this.figure = figure;
	this.modelChildren = null;

	this.commandsDeleteState = new Array();

	this.commandDelete = new draw2d.CommandDelete(figure);
}

CommandDeletePage.prototype = new draw2d.Command;

/** @private **/
CommandDeletePage.prototype.type="CommandDeletePage";


/** Execute the command the first time */
CommandDeletePage.prototype.execute=function(){
   this.redo();
}

/** Undo the command */
CommandDeletePage.prototype.redo=function(){

	var children = this.figure.getChildren();
	for (i=0; i<children.getSize(); i++)
		this.commandsDeleteState.push( new CommandDeleteState(children.get(i)) );


	$(this.commandsDeleteState).each(function(i,cmd){
		cmd.redo();
	});

	this.commandDelete.redo();
}

/** Redo the command after the user has undo this command */
CommandDeletePage.prototype.undo=function(){
	this.commandDelete.undo();

	$(this.commandsDeleteState).each(function(i,cmd){
		cmd.undo();
	});
}
