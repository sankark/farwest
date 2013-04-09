PageElement=function()
{
  /** @private **/
  this.titlebar=null;

  this.defaultBackgroundColor = new  draw2d.Color(255,255,255);
  this.highlightBackgroundColor = new  draw2d.Color(227,246,206);

  
  draw2d.CompartmentFigure.call(this);

  this.setBackgroundColor(this.defaultBackgroundColor);
  this.setDimension(150,100);
  
  this.model = new Page(this);
  this.setTitle( this.getModel().name ); 
}

PageElement.prototype = new draw2d.CompartmentFigure;

PageElement.prototype.getModel = function(){
	return this.model;
}


/**
 * @private
 **/
PageElement.prototype.createHTMLElement=function()
{
  var item = draw2d.CompartmentFigure.prototype.createHTMLElement.call(this);
  item.style.margin="0px";
  item.style.padding="0px";
  item.style.border= "1px solid black";
  item.style.cursor=null;

  this.titlebar = document.createElement("div");
  this.titlebar.style.position="absolute";
  this.titlebar.style.left   = "0px";
  this.titlebar.style.top    = "0px";
  this.titlebar.style.width = (this.getWidth()-5)+"px";
  this.titlebar.style.height = "15px";
  this.titlebar.style.margin = "0px";
  this.titlebar.style.padding= "0px";
  this.titlebar.style.fontSize="10px";
  this.titlebar.style.color="#000";
  this.titlebar.style.backgroundColor="#F2F2F2";
  this.titlebar.style.borderBottom="1px solid #000";
  this.titlebar.style.borderLeft="5px solid transparent";
  this.titlebar.style.whiteSpace="nowrap";
  this.titlebar.style.textAlign="left";
 /* this.titlebar.style.backgroundImage="url(window_toolbar.png)";*/
  this.textNode = document.createTextNode(this.title);
  this.titlebar.appendChild(this.textNode);
  item.appendChild(this.titlebar);

  
  return item;
}

PageElement.prototype.setTitle= function(title /*:String*/){
	//this.title = title;
	$(this.titlebar).text(title+ ' - Web page');
}

PageElement.prototype.fixBugCompartmentFigureResize = function(/*:draw2d.Figure*/ figure){
	if (figure.type>=1 && figure.type<=8)
		return true;
	else 
		return false;
}

PageElement.prototype.onFigureEnter = function(/*:draw2d.Figure*/ figure){
 
  if ( this.fixBugCompartmentFigureResize(figure) == true )
	return;
	
  if ( figure.getModel()!=null && figure.getModel().getType() == this.getModel().getType() )
	return;
	
  //Don't higlight if the figure already a child
  if(this.children[figure.id]==null)
     this.setBackgroundColor(this.highlightBackgroundColor);
  
  draw2d.CompartmentFigure.prototype.onFigureEnter.call(this,figure);
}

PageElement.prototype.onFigureLeave = function(/*:draw2d.Figure*/ figure){
 
  if ( this.fixBugCompartmentFigureResize(figure) == true )
	return;
  
  if ( figure.getModel()!=null && figure.getModel().getType() == this.getModel().getType() )
	return;
  
  //draw2d.CompartmentFigure.prototype.onFigureLeave.call(this,figure);
  this.setBackgroundColor(this.defaultBackgroundColor);
}

PageElement.prototype.onFigureDrop = function(/*:draw2d.Figure*/ figure){
 
  if ( this.fixBugCompartmentFigureResize(figure) == true )
	return;
  
  if ( figure.getModel()!=null && figure.getModel().getType() == this.getModel().getType() )
	return;

  //draw2d.CompartmentFigure.prototype.onFigureDrop.call(this,figure);
  this.setBackgroundColor(this.defaultBackgroundColor);
}


PageElement.prototype.setDimension=function( w /*:int*/, h /*:int*/){
  draw2d.CompartmentFigure.prototype.setDimension.call(this,w,h);
  if(this.titlebar!=null)
  {
    this.titlebar.style.width=(this.getWidth()-5)+"px";
  }
}


PageElement.prototype.getMinWidth=function(){
  return 150;
}


PageElement.prototype.getMinHeight=function(){
  return 100;
}


PageElement.prototype.setBackgroundColor= function(/*:draw2d.Color*/ color){
  this.bgColor = color;
  if(this.bgColor!=null)
    this.html.style.backgroundColor=this.bgColor.getHTMLStyle();
  else
    this.html.style.backgroundColor="transparent";
}
