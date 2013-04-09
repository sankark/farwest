/**
	@class PropertyDialog (Class)
	@Note: This class highly uses JQuery functions. Css class names refer to JQuery UI default theme.
*/
PropertyDialog = function(/*draw2d.workflow*/ workflow) {
	this.jqueryDialogId = 'propertyDialog';
	this.jqueryDialogObj = null; //return $('#'+this.jqueryDialogId)
	this.defaultSize = new Array(290,350);
	this.workflow = workflow;
	this.current_figure = null;
	this.lastActiveTab = 0;

	this.transactions = new Array();
	
	this.construct();
}

PropertyDialog.prototype.toString = function(){
	return "[propertyDialog Object]";
}

PropertyDialog.prototype.reset = function(){
	this.jqueryDialogObj.dialog('close');
	this.jqueryDialogObj.dialog( 'option','height', this.defaultSize[1] );
	this.jqueryDialogObj.dialog( 'option','width', this.defaultSize[0] );
	this.jqueryDialogObj.dialog('open');
}

PropertyDialog.prototype.setDialogTitle = function( /*:String*/ title ){
	if ( title == null )
		title = 'Properties';
	this.jqueryDialogObj.dialog( 'option', 'title', title );
}

/** @private */
PropertyDialog.prototype.construct = function(){
	var oThis = this;

	this.jqueryDialogObj = $('<div id="'+this.jqueryDialogId+'" title=""></div>').insertBefore('#paintarea');

	this.jqueryDialogObj.dialog({
		autoOpen: true,
		height: oThis.defaultSize[1],
		width: oThis.defaultSize[0],
		maxHeight: oThis.defaultSize[1]+200,
		maxWidth: oThis.defaultSize[1]+200,
		position: [ ($('#paintarea-wrapper').width()-30-280), ($('#paintarea-wrapper').position().top + 15) ]
	});

	//this.setDialogTitle(null)
	this.setNullDialog();
}

/**
	Callback da workflow
	@private
*/
PropertyDialog.prototype.onSelectionChanged=function(/*:draw2d.Figure*/ figure){
	( figure == null ) ? this.setNullDialog() : this.modifyPropertyDialog(figure)
}

PropertyDialog.prototype.setNullDialog = function(){
	var html = 'Select an element to show its properties.';
	this.jqueryDialogObj.html(html);
	this.setDialogTitle(null);
}

PropertyDialog.prototype.refresh = function(){
	this.modifyPropertyDialog( this.current_figure );
}


PropertyDialog.prototype.modifyPropertyDialog = function(/*:draw2d.Figure*/ figure){
	if ( figure==null )
		return;


	var oThis = this;
	var obj = figure.getModel();
	var properties = obj.getPropertyList();

	
	if (obj instanceof Transition){
		this.transactions = application.getElementsByClassName(Transaction);
	}

		
	var generateTR = function( obj, property, $table ){
		var html='';
		var disabled = (property.editable == false) ? ' disabled="disabled=" ' : '';
		var name = property.name;
		var HTMLinput = '';
		
		
		if ( property.property_type == 'array' ){
			disabled = ' disabled="disabled=" ';
			var value = 'Array of' + ((property.get().length == 0) ? '' : (' '+property.get()[0].getType()) ) + ' objects';
			HTMLinput = '<input type="text" '+disabled+' value="'+value+'" title="'+value+'" />';
		}
		else if ( property.property_type == 'text' ){
			var value = property.get();
			HTMLinput = '<input type="text" '+disabled+' value="'+value+'" title="'+value+'" />';
		}
		else if ( property.property_type == 'bool' ){
			var value = property.get();
			var checked = ( (new Boolean(value))==true ) ? ' checked="checked" ' : '';
			HTMLinput = '<input type="checkbox" '+disabled+' '+checked+' />';
		}
		else if ( property.property_type == 'select' ){
			if ( obj instanceof StateParameter ){
				var value = property.get();
				HTMLinput = '<select>';
				for (o=0; o<StateParameter.types.length; o++){
					var selected = ( StateParameter.types[o]==value ) ? ' selected="selected" ' : '';
					HTMLinput += '<option '+selected+' value="'+StateParameter.types[o]+'">'+StateParameter.types[o]+'</option>';
				}
				HTMLinput += '</select>';
			}
			else if ( obj instanceof GraphicalElement ){
				var value = property.get();
				HTMLinput = '<select>';
				for (o=0; o<GraphicalElement.types.length; o++){
					var selected = ( GraphicalElement.types[o]==value ) ? ' selected="selected" ' : '';
					HTMLinput += '<option '+selected+' value="'+GraphicalElement.types[o]+'">'+GraphicalElement.types[o]+'</option>';
				}
				HTMLinput += '</select>';
			}
			else if ( obj instanceof TriggerEvent ){
				var value = property.get();
				HTMLinput = '<select>';
				for (o=0; o<GraphicalElement.events.length; o++){
					var selected = ( GraphicalElement.events[o]==value ) ? ' selected="selected" ' : '';
					HTMLinput += '<option '+selected+' value="'+GraphicalElement.events[o]+'">'+GraphicalElement.events[o]+'</option>';
				}
				HTMLinput += '</select>';
			}
			else if ( obj instanceof Transition ){
				var value = (property.get() && property.get()!=null && property.get()!="null") ? property.get().getId() : null;
				
				HTMLinput = '<table style="padding:0; border:0; margin:0; border-collapse: collapse;"><tr><td width="99%">';
				HTMLinput += '<select name="transaction-select">';
				HTMLinput += '<option value="null"> &lt;None&gt; </option>';
				for (o=0; o<oThis.transactions.length; o++){
					var selected = ( oThis.transactions[o].getId()==value ) ? ' selected="selected" ' : '';
					HTMLinput += '<option '+selected+' value="'+oThis.transactions[o].getId()+'">'+oThis.transactions[o].name+'</option>';
				}
				HTMLinput += '</select>';
				HTMLinput += '</td><td>';
				HTMLinput += '<div name="new-transaction-bt" class="ui-state-default ui-corner-all addelem" title="New transaction..."><span class="ui-icon ui-icon-plus"></span></div>';
				HTMLinput += '</td></tr></table>';
			}
			else
				HTMLinput = '[unable to view]';
		}
		else{
			HTMLinput = '[unable to view]';
		}


		var $tr = $('<tr><td class="param_name">'+name+'</td><td class="param_edit">'+HTMLinput+'</td></tr>').appendTo($table);

		var setterCallback = function(e){
			var new_value = ($(e.target).attr("type")=='checkbox') ? $(e.target).attr('checked') : $(e.target).val();
			oThis.workflow.getCommandStack().execute( new CommandChangeModelProperty(property, new_value) );
		}

		$tr.find('input[type="text"]').bind('keypress',function(e){ if (e.which == 13){ setterCallback(e) }});
		$tr.find('input[type="checkbox"], select[name!="transaction-select"]').bind('change',function(e){  setterCallback(e) });
		$tr.find('div[name="new-transaction-bt"]').bind('click',function(e){ new NewTransactionDialog(); });
		$tr.find('select[name="transaction-select"]').bind('change',function(e){  
			if ( canAssignTransaction(obj, $(this).val()) == false ){
				$(this).val(0);
				MessageDialog.show('Error',"Forbidden assignment. This will result in a non-adjacent transitions' chain.","error-icon");	
			}else
				setterCallback(e);
		});
		
	}

	var generateCONTAINER = function(){
		var $wrapper = $('<div id="property_dialog_accordion"></div>');
		return $wrapper;
	}

	var generateTAB = function( title, $container, array_len ){
		var len = '';
		if (array_len && array_len!=0)
			len = ' ('+array_len+')';
		$('<h3><a href="#">'+title+len+'</a></h3>').appendTo( $container );
		return $('<div class="accordion_tab"></div>').appendTo( $container );
	}

	var generateTABLE = function( $container, obj, parentobj ){
		var $table = $('<table></table>').appendTo( $container );
		$('<tr><td class="param_name">Object type</td><td class="param_edit subtitle">'+obj.getType()+'</td></tr>').appendTo( $table );
		
		var $td = $table.find('td.param_edit.subtitle');

		if (!(obj.getParent() instanceof draw2d.Figure || obj.getParent() instanceof draw2d.Connection)){
			generateEDITBT( $td, obj, parentobj );
			generateREMBT( $td, obj, parentobj );
		}else{
			generateREMFIGBT( $td, obj );
		}

		return $table;
	}

	var generateREMFIGBT = function( $container, obj_to_remove ){
		$('<div class="ui-state-default ui-corner-all remelem" title="Remove"><span class="ui-icon ui-icon-trash"></span></div>')
			.appendTo( $container )
			.hover( function() { $(this).addClass('ui-state-hover'); },  function() { $(this).removeClass('ui-state-hover'); } )
			.click(function(e){
				new ConfirmDialog('Confirm', 'Remove '+obj_to_remove+' and its view?', function(){ 
					
					if (obj_to_remove instanceof State){
						oThis.workflow.getCommandStack().execute( new CommandDeleteState(obj_to_remove.getParent()) );
					}
					else if (obj_to_remove instanceof Page){
						oThis.workflow.getCommandStack().execute( new CommandDeletePage(obj_to_remove.getParent()) );
					}
					else if (obj_to_remove instanceof Transition){
						oThis.workflow.getCommandStack().execute( new draw2d.CommandDelete(obj_to_remove.getParent()) );
					}
					
				} );
			});
	}
	
	var generateNEWBT = function( $container, parentobj, newObjClass ){
		$('<div class="ui-state-default ui-corner-all addelem" title="Add"><span class="ui-icon ui-icon-plus"></span></div>')
			.appendTo( $container )
			.hover( function() { $(this).addClass('ui-state-hover'); },  function() { $(this).removeClass('ui-state-hover'); } )
			.click(function(e){
				if ( newObjClass == StateParameter )
					new NewStateParameterDialog( parentobj );
				else if ( newObjClass == GraphicalElement )
					new NewGraphicalElementDialog( parentobj );
				else if ( newObjClass == Action )
					new NewActionDialog( parentobj );
				else if ( newObjClass == TriggerEvent )
					new NewTriggerEventDialog( parentobj );
				else
					alert('[to implement]');
			});
	}

	var generateREMBT = function( $container, obj_to_remove, parentobj ){
		$('<div class="ui-state-default ui-corner-all remelem" title="Remove"><span class="ui-icon ui-icon-trash"></span></div>')
			.appendTo( $container )
			.hover( function() { $(this).addClass('ui-state-hover'); },  function() { $(this).removeClass('ui-state-hover'); } )
			.click(function(e){
				new ConfirmDialog('Confirm', 'Remove '+obj_to_remove+'?', function(){ 
					var allowDelete = true;
					
					if (obj_to_remove instanceof GraphicalElement)
						if (isBindedToTriggerEvents(obj_to_remove,true)==true || isBindedToAction(obj_to_remove,true)==true)
							allowDelete = false;
					
					if (obj_to_remove instanceof StateParameter)
						if (isBindedToAction(obj_to_remove,true)==true)
							allowDelete = false;
					
					if (allowDelete==true)
						oThis.workflow.getCommandStack().execute(new CommandRemoveModelChild(parentobj,obj_to_remove)) 
				} );
			});
	}
	
	var generateEDITBT = function( $container, obj, parentobj ){
		if (!(obj instanceof Action))
			return;
			
		$('<div class="ui-state-default ui-corner-all remelem" title="Edit"><span class="ui-icon ui-icon-pencil"></span></div>')
			.appendTo( $container )
			.hover( function() { $(this).addClass('ui-state-hover'); },  function() { $(this).removeClass('ui-state-hover'); } )
			.click(function(e){
				if (obj instanceof SimpleDbInsertAction)
					new SimpleDbActionDialog(parentobj,'insert',obj);
				else if (obj instanceof SimpleDbDeleteAction)
					new SimpleDbActionDialog(parentobj,'delete',obj);
				else if (obj instanceof SimpleDbOnewayDeleteAction)
					new SimpleDbActionDialog(parentobj,'oneway_delete',obj);
				else if (obj instanceof SimpleDbOnewayInsertAction)
					new SimpleDbActionDialog(parentobj,'oneway_insert',obj);
				else
					alert('Error: edit what?!')
			});
	}
	

	
	
	$con = generateCONTAINER();
	this.jqueryDialogObj.html($con);
	$tab = generateTAB ( 'General', $con );
	$tbl = generateTABLE ( $tab, obj, null );
	for (i=0; i < properties.length; i++){
		generateTR ( obj, properties[i], $tbl );
	}
	for (i=0; i < properties.length; i++){
		if ( properties[i].property_type == 'array' ){
			$tab2 = generateTAB ( properties[i].name , $con, properties[i].get().length );
			generateNEWBT ( $tab2, obj ,properties[i].relatedClass );
			for (k=0; k < properties[i].get().length; k++){
				obj2 = properties[i].get()[k];
				properties2 = obj2.getPropertyList();
				$tbl2 = generateTABLE ( $tab2, obj2, obj );
				for (m=0; m < properties2.length; m++){
					generateTR ( obj2, properties2[m], $tbl2 );
				}
				//generateEDITBT( $tab2, obj2, obj );
				//generateREMBT( $tab2, obj2, obj );
			}
		}
	}


	//this.setDialogTitle( obj.name );

	if (figure.getModel && figure.getModel()!=null && this.current_figure!=null && this.current_figure.getModel && this.current_figure.getModel()!=null){
		if (!( (figure.getModel() instanceof State && this.current_figure.getModel() instanceof State) ||
			 (figure.getModel() instanceof Page && this.current_figure.getModel() instanceof Page) ||
			 (figure.getModel() instanceof Transition && this.current_figure.getModel() instanceof Transition) ))
			this.lastActiveTab = 0;
	}else
		this.lastActiveTab = 0;

	$con.accordion({
		fillSpace: true,
		active: this.lastActiveTab,
		change: function(){ oThis.lastActiveTab = $(this).accordion('option', 'active') }
	});

	this.jqueryDialogObj.parent().find('h3,a,td.param_name,p,strong,span').addClass("unselectable");

	this.current_figure = figure;
}
