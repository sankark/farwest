TransitionElement=function( source, target ){

	draw2d.Connection.call(this);

	//To change the color when the transition is selected
	source.parentNode.getWorkflow().addSelectionListener(this);


	this.setRouter( new TransitionRouter(source, target, this) );


	this.setColor( new draw2d.Color('#848484') );

	this.transitionname_label = new draw2d.Label( '?' );
	this.transitionname_label.setBackgroundColor( new draw2d.Color(229,238,204));
	this.transitionname_label.setBorder( new draw2d.LineBorder(1) );
	this.transitionname_label.setColor( new draw2d.Color('#2E2E2E') );
	this.transitionname_label.setFontSize(7);
	
	this.addFigure( this.transitionname_label, new TransitionLabelLocator(this,2) );


	this.model = new Transition( this, source.parentNode.getModel(), target.parentNode.getModel() );
	this.setTitle( this.getModel().name );

}

TransitionElement.prototype = new draw2d.Connection;

TransitionElement.prototype.getModel = function(){
	return this.model;
}

TransitionElement.prototype.setTitle = function(/*:String*/ title){
	var trnac_text = (this.getModel().transaction!=null) ? ("[T:"+this.getModel().transaction.name+"]\n") : "";
	this.transitionname_label.setText( trnac_text + title );
}

/**
	Callback da workflow
	@private
*/
TransitionElement.prototype.onSelectionChanged=function(figure){
	if ( figure == this ){
		this.setColor( new draw2d.Color('#04B404') );
		this.transitionname_label.setColor( this.getColor() );
		this.getWorkflow().moveFront(this);
	}
	else{
		this.setColor( new draw2d.Color('#848484') );
		this.transitionname_label.setColor( new draw2d.Color('#000000') );
	}
}


/**
 * Callback 
 **/
TransitionElement.prototype.getContextMenu=function()
{
  var menu =new draw2d.Menu();
  var oThis = this;
  
  this.getWorkflow().setCurrentSelection(null);
  this.getWorkflow().setCurrentSelection(this);
  
  menu.appendMenuItem(new draw2d.MenuItem("Delete "+oThis.getModel().name, (window.imgs_path+"delete_icon.png"), function(){
	oThis.workflow.getCommandStack().execute( new draw2d.CommandDelete(oThis) );
  }));

  return menu;
}