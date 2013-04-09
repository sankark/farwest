/**
	This class inherit from draw2d.Command. Its funtion is to support undo/redo mechanism.
	Examples: Removing a StateParameter from its State.
	@params parent and child are objects inherited from GenericModel class
	@class
*/
CommandRemoveModelChild=function( parent, child ){

   draw2d.Command.call(this,"add a child to a model");

   this.parent = parent;
   this.child = child;
}

CommandRemoveModelChild.prototype = new draw2d.Command;

/** @private **/
CommandRemoveModelChild.prototype.type="CommandRemoveModelChild";


/** Execute the command the first time */
CommandRemoveModelChild.prototype.execute=function(){
   this.redo();
}

/** Undo the command */
CommandRemoveModelChild.prototype.redo=function(){
	this.parent.removeChild( this.child );
	this.child.parent = null;
}

/** Redo the command after the user has undo this command */
CommandRemoveModelChild.prototype.undo=function(){
   this.parent.addChild( this.child );
   this.child.parent = this.parent;
}
