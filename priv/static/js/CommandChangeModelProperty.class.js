/**
	This class inherit from draw2d.Command. Its funtion is to support undo/redo mechanism also for property changings.
	Examples: Changing the name of a State, the fact it's initial or not, the page it belong to, ecc.
	@class
*/
CommandChangeModelProperty=function( /*:Property*/ property, /*:Object*/ value){

   draw2d.Command.call(this,"model property change");

   this.property = property;


   this.old_value = this.property.get();
   this.new_value = value;
}

CommandChangeModelProperty.prototype = new draw2d.Command;

/** @private **/
CommandChangeModelProperty.prototype.type="CommandChangeModelProperty";


/** Execute the command the first time */
CommandChangeModelProperty.prototype.execute=function(){
   this.redo();
}

/** Undo the command */
CommandChangeModelProperty.prototype.redo=function(){
   this.property.set( this.new_value );
}

/** Redo the command after the user has undo this command */
CommandChangeModelProperty.prototype.undo=function(){
   this.property.set( this.old_value );
}
