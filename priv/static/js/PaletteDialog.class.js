/** 
	@class PaletteDialog (Class) 
	@Note: This class highly uses JQuery functions. Css class names refer to JQuery UI default theme.
*/		
PaletteDialog = function (/*draw2d.workflow*/ workflow) {
    var oThis = this;
	this.defaultSize = new Array(123,200);
	this.jqueryDialogId = "paletteDialog";
	this.jqueryDialogObj = null;
	this.workflow = workflow;

	var createElement = function(tool_element, drag_stop_event) {
		var tool_id = $(tool_element).attr('id');
		var element_to_add = null;
		
		if ( tool_id == 'tool_state' ){
			element_to_add = new StateElement();		
		}else if ( tool_id == 'tool_page' ){
			element_to_add = new PageElement();	
		}else{
			alert('id sconosciuto non so cosa inserire in workflow');
		}
		
		if ( element_to_add != null ){
			var e = drag_stop_event;
			var offsetTop = $('#paintarea-wrapper').offset().top;
			var offsetLeft = 0;
			var x;
			var y;
			
			y = ( (e.pageY > offsetTop) ? (e.pageY - offsetTop) : (offsetTop) ) - element_to_add.getHeight()/2;
			x = ( (e.pageX > offsetLeft) ? (e.pageX - offsetLeft) : (offsetLeft) ) - element_to_add.getWidth()/2;
			
			x = x + $('#paintarea-wrapper').scrollLeft();
			y = y + $('#paintarea-wrapper').scrollTop();
			
			if ( element_to_add.getModel() instanceof State)
				oThis.workflow.getCommandStack().execute( new CommandAddState(oThis.workflow,element_to_add,x,y) );
			else
				oThis.workflow.getCommandStack().execute( new draw2d.CommandAdd(oThis.workflow,element_to_add,x,y) );
		}
		
    }
	
	// Callback per il meccanismo di dragging di jquery. Help to fix problems with z-index during jquery drag and drop mechanism
	var getDraggableHelper = function(){	
		var tool_id = $(this).attr('id');
		var tool_width = $('#'+tool_id).css('width');
		var $helper = $('#'+tool_id).clone();
		if (! $helper.parent().is('#paintarea') ){ $helper.appendTo('#paintarea'); }
		//$helper.css('z-index',20000);
		//$helper.css('width', tool_width);
		$helper.addClass('tool_draggable unselectable');
		return $helper;
	}

  
	var getHTMLTool = function(tool_id, bt_caption){
		return '<div class="tool_draggable ui-state-default ui-corner-all" id="'+tool_id+'" ><table><tr><td><img alt="" src="'+imgs_path+'tool_add.png"/></td><td>&nbsp;'+bt_caption+'</td></tr></table></div>';
	}
	
	var initialize = function(){
		oThis.jqueryDialogObj = $('<div id="'+oThis.jqueryDialogId+'" title="Palette">Components</div>').insertBefore('#paintarea');
	
		oThis.jqueryDialogObj.dialog({
				autoOpen: true,
				dialogClass: 'unselectable',
				height: oThis.defaultSize[1],
				width: oThis.defaultSize[0],
				maxHeight: oThis.defaultSize[1]+200,
				maxWidth: oThis.defaultSize[1]+200,
				resizable: false,
				position: [10,($('#paintarea-wrapper').position().top+35)]
		});
	
	
		var html='';
		html += '<div id="tools_container">';
	    html += getHTMLTool('tool_state','State');
		html += getHTMLTool('tool_page','Web Page');
		html += '<span id="bt_tool_undo" class="ui-state-default ui-state-disabled ui-corner-all" ><img alt="" src="'+imgs_path+'tool_undo.png"/>Undo</span>';
		html += '<span id="bt_tool_redo" class="ui-state-default ui-state-disabled ui-corner-all" ><img alt="" src="'+imgs_path+'tool_redo.png"/>Redo</span>';
		html += '</div>';
		
		 
		oThis.jqueryDialogObj.html(html);
		
		$('#tools_container span[id="bt_tool_undo"]').bind('click',function(e){ if ($(this).hasClass('ui-state-disabled')) return false; oThis.workflow.getCommandStack().undo(); });
		$('#tools_container span[id="bt_tool_redo"]').bind('click',function(e){ if ($(this).hasClass('ui-state-disabled')) return false; oThis.workflow.getCommandStack().redo(); });
		
		$('.tool_draggable').draggable({ 
			helper: getDraggableHelper,
			delay: 100,
			//scope: 'perpl',
			//revert: 'invalid',
			stop: function(e, ui) { createElement(this,e); }
		});
		
		/*
		$('#paintarea').droppable({
			scope: 'perpl'
		});
		*/
		
		$('#tools_container .ui-state-default').hover(
			function() { if ($(this).hasClass('ui-state-disabled')) return false; $(this).addClass('ui-state-hover'); }, 
			function() { if ($(this).hasClass('ui-state-disabled')) return false; $(this).removeClass('ui-state-hover'); }
		);
	}
	
	initialize();
}

PaletteDialog.prototype.reset = function(){
	this.jqueryDialogObj.dialog('close');
	this.jqueryDialogObj.dialog( 'option','height', this.defaultSize[1] );
	this.jqueryDialogObj.dialog( 'option','width', this.defaultSize[0] );
	this.jqueryDialogObj.dialog('open');
}

/** @private */
PaletteDialog.prototype.onSetDocumentDirty = function(){
	
	var setEnabled = function( value,elem ){
		if ( value == true ){ 
			$(elem).removeClass('ui-state-disabled'); 
		}else{
			$(elem).addClass('ui-state-disabled'); 
		}
	}
	
	setEnabled( this.workflow.getCommandStack().canUndo(), $('#tools_container span[id="bt_tool_undo"]') );
	setEnabled( this.workflow.getCommandStack().canRedo(), $('#tools_container span[id="bt_tool_redo"]') );
}