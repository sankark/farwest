/**
	This class inherit from draw2d.Command. Its funtion is to support undo/redo mechanism.

	@class
*/

CommandAddState=function(workflow,figure,x,y){
	this.figure = figure;
	this.page = null;
	this.commandAdd = new draw2d.CommandAdd(workflow,figure,x,y);
}

CommandAddState.prototype = new draw2d.Command;

/** @private **/
CommandAddState.prototype.type="CommandAddState";


/** Execute the command the first time */
CommandAddState.prototype.execute=function(){
   this.redo();
}

/** Undo the command */
CommandAddState.prototype.redo=function(){

	this.commandAdd.redo();

	var state = this.figure.getModel();
	this.page = this.getPageOnDrop(this.figure);
	state.page = this.page;

	if ( this.page!=null ){
		this.page.addChild( state );
		this.page.getParent().addChild( state.getParent() );
		this.figure.parent = this.page.getParent();
	}
	//fix title bug
	this.figure.setTitle( state.name );

}

/** Redo the command after the user has undo this command */
CommandAddState.prototype.undo=function(){

	this.commandAdd.undo();

	var state = this.figure.getModel();
	state.page = null;

	if ( this.page!=null ){
		this.page.removeChild( state );
		this.page.getParent().removeChild( state.getParent() );
		this.figure.parent = null;
	}
	
	//fix title bug
	this.figure.setTitle( state.name );
}


/** @private */
CommandAddState.prototype.getPageOnDrop = function(figure){

		var stateX = $( figure.getHTMLElement() ).offset().left;
		var stateY = $( figure.getHTMLElement() ).offset().top;
		var stateW = $( figure.getHTMLElement() ).width();
		var stateH = $( figure.getHTMLElement() ).height();
		var tolerance = 8;

		var founded = new Array();
		$( application.getElementsByClassName(Page) ).each(function(i,page){
			var pageX = $( page.getParent().getHTMLElement() ).offset().left;
			var pageY = $( page.getParent().getHTMLElement() ).offset().top;
			var pageW = $( page.getParent().getHTMLElement() ).width();
			var pageH = $( page.getParent().getHTMLElement() ).height();
			var margL = stateX - pageX;
			var margR = (pageX + pageW) - (stateX + stateW);
			var margT = stateY - pageY;
			var margB = (pageY + pageH) - (stateY + stateH);
			if ( margL>-tolerance &&  margR>-tolerance &&  margT>-tolerance &&  margB>-tolerance ){
				var margins = new Array(page,margL,margR,margT,margB);
				founded.push(margins);
			}
		});

		var min = null;
		var founded_page = null;
		$( founded ).each(function(i,margin){
			var sum = margin[1]+margin[2]+margin[3]+margin[4];
			if (sum<min | min==null){
				min = sum;
				founded_page = margin[0];
			}
		});

		return founded_page;
	}
