StateElement=function(){
	this.titleNode = null;
	this.radius = 70;

	draw2d.Circle.call(this);


	this.setResizeable(false);
    this.portDx = null;
	this.portSn = null;


	this.setDimension(this.radius,this.radius);

	this.model = new State(this);
	this.setTitle( this.getModel().name );
}

StateElement.prototype = new draw2d.Circle;

StateElement.prototype.getModel = function(){
	return this.model;
}

StateElement.prototype.setWorkflow=function(/*:Workflow*/ workflow ){

  draw2d.Circle.prototype.setWorkflow.call(this,workflow);

  if(workflow==null)
    return;

  if( workflow!=null && this.portSn==null ){
    this.portSn = new StatePort();
    this.portSn.setWorkflow(workflow);
    this.portSn.setBackgroundColor( new draw2d.Color(11,59,11) );
    this.portSn.setColor( null );
	this.portSn.setName("portSn");
	//this.portSn.setProperty("type","state");
    this.addPort(this.portSn,0,this.height/2);
  }

  if( workflow!=null && this.portDx==null ){
    this.portDx = new StatePort();
    this.portDx.setWorkflow(workflow);
    this.portDx.setBackgroundColor( new draw2d.Color(11,59,11) );
    this.portDx.setColor( null );
	this.portDx.setName("portDx");
    this.addPort(this.portDx,this.width,this.height/2);
  }

}


StateElement.prototype.createHTMLElement=function(){

	var item = draw2d.Circle.prototype.createHTMLElement.call(this);

	this.titleNode = document.createElement('p');
	this.titleNode.id = this.id + "_titleNode";
	this.titleNode.style.textAlign="center";
	this.titleNode.style.position="absolute";
	this.titleNode.style.fontSize="10px";
	this.titleNode.style.top = (this.radius/2 -7)+'px';
	this.titleNode.style.left = '2px';
	this.titleNode.style.width = (this.radius-5)+'px';
	this.titleNode.style.height = (this.radius/2)+'px';
	this.titleNode.style.overflow = "hidden";
	this.titleNode.style.margin="0px";
	this.titleNode.style.zIndex = ""+ ( draw2d.Figure.ZOrderBaseIndex + 1 );

	item.appendChild( this.titleNode );

	this.setBackgroundColor( new draw2d.Color(241,241,237) );
	this.setColor( new draw2d.Color(88,88,88) )

    return item;
}

StateElement.prototype.setTitle = function(/*String*/ title){
	$( this.getHTMLElement() ).find('p[id="'+this.id+'_titleNode"]').text( title );
}


/**
 * Callback 
 **/
StateElement.prototype.getContextMenu=function()
{
  var menu =new draw2d.Menu();
  var oThis = this;
  
  this.getWorkflow().setCurrentSelection(null);
  this.getWorkflow().setCurrentSelection(this);
  
  menu.appendMenuItem(new draw2d.MenuItem("Delete "+oThis.getModel().name, (window.imgs_path+"delete_icon.png"), function(){
	oThis.workflow.getCommandStack().execute( new CommandDeleteState(oThis) );
  }));
  
  menu.appendMenuItem(new draw2d.MenuItem("Set as initial", null, function(){
	oThis.getModel().setInitial();
	window.propertyDialog.refresh();
  }));

  return menu;
}

