/**
	@class ElementsListDialog (Class)
	@note Note: This dialog is mainly for debug purpose.
*/
ElementsListDialog = function( /*:draw2d.workflow*/ workflow ){
	this.jqueryDialogId = 'elementsListDialog';
	this.jqueryDialogObj = null; //return $('#'+this.jqueryDialogId)
	this.workflow = workflow;

	this.construct();
}

/** @private */
ElementsListDialog.prototype.construct = function(){
	var oThis = this;

	this.jqueryDialogObj = $('<div id="'+this.jqueryDialogId+'"></div>').insertBefore('#paintarea');

	this.jqueryDialogObj.dialog({
		autoOpen: false,
		title: 'Elements list',
		zIndex: 9999,
		height: 300,
		width: 370,
		minWidth: 100,
		maxWidth: 600,
		minHeight: 100,
		maxHeight: 800,
		position: [150,110]
	});

	/*
	this.jqueryDialogObj.bind('dialogfocus', function(event, ui) {
		oThis.workflow.setCurrentSelection(null);
	});
	*/

	this.refresh();
}

ElementsListDialog.prototype.show = function(){
	this.jqueryDialogObj.dialog('open');
}

ElementsListDialog.prototype.close = function(){
	this.jqueryDialogObj.dialog('close');
}

ElementsListDialog.prototype.refresh = function(){
	var html = '';
	var elements = application.getElements();
	var count = elements.length;
	var oThis = this;

	html += '<p>Here is a list of all the elements involved in your diagram. At the current version this dialog is mainly for debug purpose. <a href="#">[Refresh]</a></p>';
	
	var print_element = function(e){
			var code='<div class="obj"><strong style="color:green;">'+e+'</strong>';

			code += '<br/><span class="attr1">model parent:</span> '+e.getParent();
			code += '<br/><span class="attr1">id:</span> '+e.getId();
			
			if ( e.getChildren().length != 0 ){
				code += '<br/><span class="attr1">model children:</span><ul>';
				for (c=0; c<e.getChildren().length; c++)
					code += ( '<li>'+ e.getChildren()[c] );
				code += '</ul>'
			}
			
			

			if ( e instanceof Page ){
				if ( e.getParent().getChildren().getSize() != 0 ){
					code += '<br/><span class="attr1">draw2d children:</span><ul>';
					for (c=0; c<e.getParent().getChildren().getSize(); c++)
						code += ( '<li>'+ e.getParent().getChildren().get(c).getModel() );
					code += '</ul>';
				}
			}

        if ( e instanceof Template ){
            if ( e.getParent().getChildren().getSize() != 0 ){
                code += '<br/><span class="attr1">draw2d children:</span><ul>';
                for (c=0; c<e.getParent().getChildren().getSize(); c++)
                    code += ( '<li>'+ e.getParent().getChildren().get(c).getModel() );
                code += '</ul>';
            }
        }

			else if ( e instanceof State ){
				/*
				code += '<br/><span class="attr1">draw2d ports </span>('+(e.getParent().getPorts().getSize())+'): ';
				for (c=0; c<e.getParent().getPorts().getSize(); c++)
					code += e.getParent().getPorts().get(c).getName() + '; ';
				*/
				code += ( '<br/><span class="attr1">Parent page:</span> '+ e.page ) ;
			}

			code+='</div>';
			return code;
	}
	
	if ( count >=1 ){
	
		for (i=0; i<count; i++){
			var e = elements[i];
			html+=print_element(e);
		}

	}

	this.jqueryDialogObj.html( html );
	this.jqueryDialogObj.find('a').click( function(e){ e.preventDefault(); oThis.refresh(); } );

	if ( this.jqueryDialogObj.dialog('isOpen') )
		this.jqueryDialogObj.dialog('moveToTop');
}


