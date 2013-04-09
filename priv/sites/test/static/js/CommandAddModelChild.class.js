/**
	This class inherit from draw2d.Command. Its funtion is to support undo/redo mechanism.
	Examples: Adding a StateParameter to a State.
	@params parent and child are objects inherited from GenericModel class
	@class
*/
CommandAddModelChild=function( parent, child ){

   draw2d.Command.call(this,"add a child to a model");

   this.parent = parent;
   this.child = child;
}

CommandAddModelChild.prototype = new draw2d.Command;

/** @private **/
CommandAddModelChild.prototype.type="CommandAddModelChild";


/** Execute the command the first time */
CommandAddModelChild.prototype.execute=function(){
   this.redo();
}

/** Undo the command */
CommandAddModelChild.prototype.redo=function(){
	this.parent.addChild( this.child );
	this.child.setParent( this.parent );
}

/** Redo the command after the user has undo this command */
CommandAddModelChild.prototype.undo=function(){
   this.parent.removeChild( this.child );
   this.child.setParent( null );
}
