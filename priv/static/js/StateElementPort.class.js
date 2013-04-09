StatePort=function(/*:Figure*/ uiRepresentation){

  draw2d.Port.call(this, uiRepresentation);

}

StatePort.prototype = new draw2d.Port; //spwnd!

StatePort.prototype.getModel = function(){
	return this.model;
}

StatePort.prototype.onDrop = function(/*:Port*/ port){

	/*
	if(port.getMaxFanOut && port.getMaxFanOut() <= port.getFanOut())
		return;
	*/

	var source = this;
	var target = port;

	var source_type = source.parentNode.getModel().getType();
	var target_type = target.parentNode.getModel().getType();

	if( source_type == target_type ){
		var command = new draw2d.CommandConnect( this.parentNode.workflow, source, target );
		command.setConnection( new TransitionElement(source, target) );
		this.parentNode.workflow.getCommandStack().execute(command);
	}

}
