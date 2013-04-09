/**
	Confirm Dialog
	The obj is destroyed after clicking Ok or Cancel so call this class in a way like
	new ConfirmDialog('Confirm','<p>save?</p>',function(){...},function(){...});

	@Note: This class highly uses JQuery functions. Css class names refer to JQuery UI default theme.
	@class
*/
ConfirmDialog = function( /*:String*/ title, /*:String*/ html, /*:Function*/ yesCallback, /*:Function*/ noCallback, /*:Array*/ dimensions ){

	this.jqueryDialogObj = null;
	this.yesCallback = (yesCallback) ? yesCallback : null;
	this.noCallback  = (noCallback)  ? noCallback  : null;
	this.dimensions  = (dimensions)  ? dimensions  : [400,160];
	

	this.show( title, html, this.yesCallback, this.noCallback, this.dimensions);
}


ConfirmDialog.prototype.show = function( title, html, yesCallback, noCallback ,dimensions){
	var oThis = this;
	
	this.noCallback = (noCallback && noCallback instanceof Function) ? noCallback : ((this.noCallback==null) ? (function(){}) : this.noCallback);
	this.yesCallback = (yesCallback && yesCallback instanceof Function) ? yesCallback : ((this.yesCallback==null) ? (function(){}) : this.yesCallback);
	this.dimensions = (dimensions && dimensions instanceof Array) ? dimensions : ((this.dimensions instanceof Array) ? this.dimensions : [400,160]);
			

		
	if (this.jqueryDialogObj==null){
	
		this.jqueryDialogObj = $('<div id="confirmDialog">'+html+'</div>').insertBefore("#paintarea");
	
		this.jqueryDialogObj.dialog({
			autoOpen: true,
			zIndex: 30000,
			width: this.dimensions[0],
			height: this.dimensions[1],
			dialogClass: 'unselectable',
			modal: true,
			stack: true,
			title: title,
			buttons: {
				"Ok": function() {
					if ( oThis.yesCallback.call(this) != false )
						oThis.dispose();
				},
				"Cancel": function() {
					if ( oThis.noCallback.call(this) != false )
						oThis.dispose();
				}
			}
		});	
		
	}else{		
		this.jqueryDialogObj.dialog('option', 'title', title);
		this.jqueryDialogObj.html( html );
	}
}

/** @private */
ConfirmDialog.prototype.dispose = function(){
	this.jqueryDialogObj.dialog("close");
	this.jqueryDialogObj.dialog("destroy");
	this.jqueryDialogObj.remove();
	this.jqueryDialogObj = null;
	this.yesCallback = null;
	this.noCallback = null;
}

/** @static */
ConfirmDialog.getInputValue = function( $input , _checkForbiddenChrs, _trimValue){	
	var trimValue = (_trimValue && (_trimValue=='false' || _trimValue==false)) ? false : true;
	var checkForbiddenChrs = (_checkForbiddenChrs && (_checkForbiddenChrs=='false' || _checkForbiddenChrs==false)) ? false : true;
	var isValid = true;
	var value = (trimValue==true) ? $.trim($input.val()) : $input.val();

	if (checkForbiddenChrs==true){
		if (value.length == 0 | !value.isClean())
			isValid = false;
	}else{
		if (value.length == 0)
			isValid = false;
	}
	
	if ( isValid == false ){
		$input.effect("pulsate", { times:1 }, 500);
		return null;
	}
	
	return value;
}

/*==============================================================================================================================*/
/**
	@class InputDialog
*/
InputDialog = function(title,message,yesCallback){

	var html = message+'<br/><input type="text" name="_txt" maxlength="300" style="width:100%"/>';

	var _yesCallback = function(){
		var txt = ConfirmDialog.getInputValue( $(this).find('input[type="text"][name="_txt"]') );

		if (txt==null)
			return false;
		else
			yesCallback(txt);
	}

	new ConfirmDialog(title,html,_yesCallback,null,[400,200]);
}

/*==============================================================================================================================*/
/**
	@class New StateParameter Dialog
*/
NewStateParameterDialog = function(/*:GenericModel*/ parent){

	var html = '<p>Add a new parameter to <strong>'+parent.name+'</strong>.</p>'
	html += '<table><tr><td>Name:</td><td><input type="text" name="_name"/></td></tr>';
	html += '<tr><td>Type:</td><td><select name="_type">';
	for (o=0; o<StateParameter.types.length; o++){
		html += '<option value="'+StateParameter.types[o]+'">'+StateParameter.types[o]+'</option>';
	}
	html += '</td></select></tr></table>';

	var yesCallback = function(){
		var name = ConfirmDialog.getInputValue( $(this).find('input[type="text"][name="_name"]') );
		var type = ConfirmDialog.getInputValue( $(this).find('select[name="_type"]') );

		if (name==null | type==null)
			return false;
		else
			parent.getParent().getWorkflow().getCommandStack().execute( new CommandAddModelChild(parent,new StateParameter(parent,name,type)) );
	}

	new ConfirmDialog('New',html,yesCallback,null,[350,200]);
}


/*==============================================================================================================================*/
/**
	@class New GraphicalElement Dialog
*/
NewGraphicalElementDialog = function(/*:GenericModel*/ parent){

	var html = '<p>Add a new graphical element to <strong>'+parent.name+'</strong>.</p>'
	html += '<table><tr><td>Name:</td><td><input type="text" name="_name"/></td></tr>';
	html += '<tr><td>Type:</td><td><select name="_type">';
	for (o=0; o<GraphicalElement.types.length; o++){
		html += '<option value="'+GraphicalElement.types[o]+'">'+GraphicalElement.types[o]+'</option>';
	}
	html += '</td></select></tr></table>';

	var yesCallback = function(){
		var name = ConfirmDialog.getInputValue( $(this).find('input[type="text"][name="_name"]') );
		var type = ConfirmDialog.getInputValue( $(this).find('select[name="_type"]') );

		if (name==null | type==null)
			return false;
		else
			parent.getParent().getWorkflow().getCommandStack().execute( new CommandAddModelChild(parent,new GraphicalElement(parent,name,type)) );
	}

	new ConfirmDialog('New',html,yesCallback,null,[350,200]);
}


/*==============================================================================================================================*/
/**
	@class New Transaction Dialog
*/
NewTransactionDialog = function(){

	var html = '<p>Create a new Transaction</p>'
	html += '<table><tr><td>Name:</td><td><input type="text" name="_name"/></td></tr></table>';

	var yesCallback = function(){
		var name = ConfirmDialog.getInputValue( $(this).find('input[type="text"][name="_name"]') );

		if (name==null)
			return false;
		else{
			application.nonGraphicalElements.push( new Transaction(name) );
			propertyDialog.refresh();
		}
	}

	new ConfirmDialog('New',html,yesCallback,null,[350,200]);
}


/*==============================================================================================================================*/
/**
	@class New TriggerEvent Dialog
*/
NewTriggerEventDialog = function(/*:GenericModel*/ parent){

	var page = parent.source.page;
	
	if (page==null){
		MessageDialog.show('Error','<i>'+parent.source.name+'</i> must be in page.','error-icon');
		return;
	}
	
	var html = '<p>Add a new trigger event to <i>'+parent.name+'</i>.</p>'
	html += '<table><tr><td width="20%">Graphical element:</td><td>';

	html +='<select name="_grap">';
	var grs = page.getGraphicalElements()
	for (i=0; i<grs.length; i++)
		html += '<option value="'+grs[i].getId()+'">'+grs[i].graphelem_name+'</option>';
	html += '</select>'

	html += '</td></tr><tr><td>Event type:</td><td colspan=2>';

	html +='<select name="_type">';
	for (o=0; o<GraphicalElement.events.length; o++)
		html += '<option value="'+GraphicalElement.events[o]+'">'+GraphicalElement.events[o]+'</option>';
	html += '</select>'

	html += '</td></tr></table>';


	var yesCallback = function(){
		var grap_id = ConfirmDialog.getInputValue( $(this).find('select[name="_grap"]') );
		var type = ConfirmDialog.getInputValue( $(this).find('select[name="_type"]') );

		if (grap_id==null | type==null )
			return false;
		else{
			var grahElem = application.getElementById( grap_id );
			
			if (!(grahElem instanceof GraphicalElement)){
				MessageDialog.show('Exception','<i>grahElem</i> must be an instance of GraphicalElement. TriggerEvent obj could not be created.','error-icon');
				return;
			}
			
			if (isBindedToTriggerEvents(grahElem,false)==true){
				MessageDialog.show('Warning','Graphical element <i>'+grahElem+'</i> is already binded to a trigger event.','error-icon');
				return;
			}
				
			parent.getParent().getWorkflow().getCommandStack().execute( new CommandAddModelChild(parent,new TriggerEvent(grahElem,type)) );
		}
	}

	var confDiag = new ConfirmDialog('New',html,yesCallback,null,[450,200]);
}


/*=========================================================================================================================*/
/**
	MessageDialog (static object)
	usage: MessageDialog.show('Message','<p>and error occurred!</p>');
*/
var MessageDialog = function() {
    jqueryDialogObj = null;

    return{
        show:function( title, html, icon ){
					
			if (!icon)
				icon='';
			else if (icon=='success-icon')
				icon='<span class="ui-icon ui-icon-circle-check"  style="float:left; margin:0 7px 50px 0;"></span>';
			else if (icon=='error-icon')
				icon='<span class="ui-icon ui-icon-circle-close"  style="float:left; margin:0 7px 50px 0;"></span>';
				
			html = icon + html;
			
			if ( jqueryDialogObj == null ){
				jqueryDialogObj = $('<div id="messageDialog">'+( (html==null) ? 'null' : html )+'</div>').insertBefore("#paintarea");

				jqueryDialogObj.dialog({
					autoOpen: true,
					zIndex: 30000,
					dialogClass: 'unselectable',
					modal: true,
					stack: true,
					width: 400,
					height: 200,
					title: title,
					buttons: {
						"Ok": function(){
							$(this).dialog("destroy");
							$(this).remove();
							jqueryDialogObj = null;
						}
					}
				});	
			}
			else{
				jqueryDialogObj.html( html );
				jqueryDialogObj.dialog('option', 'title', title);
			}
		}
		
    }
}();


/*==============================================================================================================================*/
/**
	@class New Action Dialog
*/
NewActionDialog = function(/*:GenericModel*/ parent){
	new SelectActionDialog(parent);
}


/*==============================================================================================================================*/
/**
	@class New Action Dialog
*/
SimpleDbActionDialog = function(/*:GenericModel*/ parent, dbAction, storedAction){
	
	if (!parent instanceof Transition)
		return;
		
	if (storedAction && !storedAction instanceof Action)
		return;	
	
	//function...
	var get_sources_select = function(source){
		var source_id = (source!=null) ? source.getId() : null;
		var code = '<select name="_src">';
		$(parent.target.getParameters()).each(function(i, stateParam){
			var selected = (source_id && stateParam.getId()==source_id) ? 'selected="selected"' : '';
			code += '<option value="'+stateParam.getId()+'" '+selected+'>'+stateParam+'</option>';
		});
		if (parent.source.page != null){
			$(parent.source.page.getGraphicalElements()).each(function(i, graphElem){
				var selected = (source_id && graphElem.getId()==source_id) ? 'selected="selected"' : '';
				code += '<option value="'+graphElem.getId()+'" '+selected+'>'+graphElem+'</option>';
			});		
		}
		code += '</select>';
		return code;
	}
	
	//function...
	var yesCallback = function(){
		var can_complete = true;
		var actionParameters = new Array();
		var numOfTableNameActionParameters = 0;
		var duplicateValues = false;
		
		$($(this).find('tr[name="_trvalue"]')).each(function(){
			var name = ConfirmDialog.getInputValue($(this).find('input[name="_clmname"]'));
			var source_id = ConfirmDialog.getInputValue($(this).find('select[name="_src"]'));
			
			if (name!=null && source_id!=null){
				if (name==ActionParameter.tableName)
					numOfTableNameActionParameters++;
				actionParameters.push( new ActionParameter(name, application.getElementById(source_id)) );
			}else{
				can_complete = false;
			}

		});
		
		for (k=0; k<actionParameters.length; k++)
			for (m=0; m<actionParameters.length; m++)
				if ( m!=k && (actionParameters[k].name == actionParameters[m].name || actionParameters[k].source == actionParameters[m].source) ){
					
					duplicateValues = true;
				}
						
		if (can_complete == false){
			return false;
		}else if (actionParameters.length == 0){
			MessageDialog.show('Error',"There a no action parameters!","error-icon");
			return false;
		}
		else if (numOfTableNameActionParameters!=1){
			MessageDialog.show('Error',"An action parameter's name must be <i>TableName</i>.","error-icon");
			return false;
		}
		else if (duplicateValues==true){
			MessageDialog.show('Error',"Some action parameter have the same name/source.","error-icon");
			return false;
		}
		else{	
			if (storedAction){
				storedAction.parameters = actionParameters;
			}else{
				var newAction = null;
				
				if (dbAction=='insert')
					newAction = new SimpleDbInsertAction(actionParameters);
				else if (dbAction=='delete')
					newAction = new SimpleDbDeleteAction(actionParameters);
				else if (dbAction=='oneway_insert')
					newAction = new SimpleDbOnewayInsertAction(actionParameters);
					
				parent.getParent().getWorkflow().getCommandStack().execute( new CommandAddModelChild(parent,newAction) );
			}
			return true;
		}
	}

	
	//function...
	var addActionParameterRow = function(diag,_storedActionParameter){
		var storedActionParameter = (_storedActionParameter && _storedActionParameter instanceof ActionParameter) ? _storedActionParameter : null;
		
		var name = (storedActionParameter!=null) ? storedActionParameter.name : '';
		var source = (storedActionParameter!=null) ? storedActionParameter.source : null;
		
		var html = '<tr name="_trvalue"><td><input type="text" name="_clmname" value="'+name+'"/></td><td>'+get_sources_select(source)+'</td><td>'+bt_remove+'</td></tr>';
		
		var $tr = $(html).appendTo(diag.jqueryDialogObj.find('table[name="_tblvalues"]'));
	
		if (name==ActionParameter.tableName)
			$tr.find('input[name="_clmname"]').attr("readonly",true).css("color","green");
			
		$tr.find('div[name="_remvalue"]').click(function(){$tr.remove();});
	
		return $tr;
	}
		
		
	var bt_remove = '<div name="_remvalue" class="ui-state-default ui-corner-all remelem" title="Remove"><span class="ui-icon ui-icon-minus"></span></div>';
	var bt_add = '<div name="_addvalue" class="ui-state-default ui-corner-all addelem" title="Add"><span class="ui-icon ui-icon-plus"></span></div>';
	
	var title = '?';
	var windowTitle = '?';
	
	if (dbAction=='insert' || dbAction=='oneway_insert'){
		title = 'This generates an sql query like "<i>Insert into {tablename} values(\'{val1}\',\'{val2}\',...);"</i>';
		windowTitle = 'Simple DB insert';
	}else if (dbAction=='delete'){
		title = 'This generates an sql query like "<i>Delete from {tablename} where(\'{val1}\',\'{val2}\',...);"</i>';
		windowTitle = 'Simple DB delete';
	}
		
	var html = '<p>Add a new action to <i>'+parent.name+'</i>. '+title+'</p><br/>';
	    html += '<table name="_tblvalues"><tr><td>ActionParamente\'s name</td><td>ActionParamenter\'s value</td><td>'+bt_add+'</td></tr></table>';
	
	var diag = new ConfirmDialog(windowTitle, html, yesCallback, null, [450,380]);
		diag.jqueryDialogObj.find('div[name="_addvalue"]').click(function(){ addActionParameterRow(diag) });

	if (storedAction){
	
		//add a table row for each of the action parameters sons of the stored action
		$(storedAction.getParameters()).each(function(){
			addActionParameterRow(diag,this);
		});
		
	}else{
	
		//add some trable row for example
		addActionParameterRow(diag,null).find('input[name="_clmname"]').val(ActionParameter.tableName).attr("readonly",true).css("color","green");
		addActionParameterRow(diag);
		addActionParameterRow(diag);
	}

}



/*=========================================================================================================================*/
/**
	Select action dialog
*/
SelectActionDialog = function(/*:GenericModel*/ parent){
	
	var html = 'Choose an action:<br/>';
		html += '<select>';
		html += '<option value="Simple_DB_insert">Simple DB insert</option>';
		html += '<option value="Simple_DB_oneway_insert">Simple DB insert (OneWay)</option>';
		html += '<option value="Simple_DB_delete">Simple DB delete</option>';
		html += '</select>';
	
	var yesCallback = function(){
		var choosedAction = ConfirmDialog.getInputValue($(this).find('select'));
		
		if (choosedAction=='Simple_DB_insert')
			new SimpleDbActionDialog(parent,'insert');
		else if (choosedAction=='Simple_DB_delete')
			new SimpleDbActionDialog(parent,'delete');
		else if (choosedAction=='Simple_DB_oneway_insert')
			new SimpleDbActionDialog(parent,'oneway_insert');
		else{}
		
	}
	
    new ConfirmDialog('New action',html,yesCallback,null,[350,200]);
	
}

/*=========================================================================================================================*/
/**
	Select action dialog
*/
DbConnectionDescriptorDialog = function(dbConnectionDescriptor){
	
	if ( (!(dbConnectionDescriptor)) || (!(dbConnectionDescriptor instanceof DbConnectionDescriptor)) )
		return;
	
	var html = '<div name="DbConnectionDescriptorDialog">';
	html += '<p>Change database connection parameters of your project:</p>';
	html += '<table>';
	html += '<tr><td>Driver classname:</td><td><input name="_driver" type="text" value="'+(dbConnectionDescriptor.driver)+'" /></td></tr>';
	html += '<tr><td>Connection url:</td><td><input name="_url" type="text" value="'+(dbConnectionDescriptor.url)+'" /></td></tr>';
	html += '<tr><td>Username:</td><td><input name="_user" type="text" value="'+(dbConnectionDescriptor.user)+'" /></td></tr>';
	html += '<tr><td>Password:</td><td><input name="_pass" type="password" value="'+(dbConnectionDescriptor.pass)+'" /></td></tr>';
	html += '</table>';
	html += '</div>';
	
	var yesCallback = function(){
		var driver = ConfirmDialog.getInputValue($(this).find('input[name="_driver"]'),'false');
		var url = ConfirmDialog.getInputValue($(this).find('input[name="_url"]'),'false');
		var user = ConfirmDialog.getInputValue($(this).find('input[name="_user"]'),'false','false');
		var pass = ConfirmDialog.getInputValue($(this).find('input[name="_pass"]'),'false','false');
		
		if (driver==null || url==null || user==null || pass==null )
			return false;
		else{
			dbConnectionDescriptor.driver = driver;
			dbConnectionDescriptor.url = url;
			dbConnectionDescriptor.user = user;
			dbConnectionDescriptor.pass = pass;
			return true;
		}
	}
	
    new ConfirmDialog('Configure database parameters',html,yesCallback,null,[400,300]);	
}

