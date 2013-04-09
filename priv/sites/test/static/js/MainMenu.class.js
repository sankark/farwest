/**
	@class MainMenu (Class)
	@Note: This class highly uses JQuery functions. Css class names refer to JQuery UI default theme.
*/
MainMenu = function(/*:draw2d.workflow*/ workflow){
	this.mainMenuContainerId = 'mainMenu';
	this.workflow = workflow;

	this.construct();
}

/** @private */
MainMenu.prototype.construct = function(){
	var oThis = this;
	$('#header').after('<div id="'+this.mainMenuContainerId+'" class="mainmenu-container ui-corner-bl ui-corner-br" ><ul></ul></div>');

	this.addButton('Open', 0, function(){ XMLImport.list()    } );
	this.addButton('Save', 1, function(){ XMLExport.execute() } );
	this.addButton('Save as...', 1, function(){ XMLExport.execute(true) } );
	this.addButton('Open object\'s list',2,function(){ if (window.elementsListDialog) elementsListDialog.show(); });
	this.addButton('Reset to default view',6,function(){ if (window.elementsListDialog) elementsListDialog.close(); propertyDialog.reset(); paletteDialog.reset(); oThis.getWorkflow().setCurrentSelection(null) });
	this.addSeparator();
	this.addButton('Move figure back',4, function(){if (oThis.getWorkflow().getCurrentSelection()!=null) oThis.getWorkflow().moveBack(oThis.getWorkflow().getCurrentSelection())} );
	this.addButton('Move figure front',5, function(){if (oThis.getWorkflow().getCurrentSelection()!=null) oThis.getWorkflow().moveFront(oThis.getWorkflow().getCurrentSelection())} );
	this.addSeparator();
	this.addButton('Configure database parameters', 7, function(){ 
		var dbConnectionDescriptors = application.getElementsByClassName(DbConnectionDescriptor);
		var mainDbConnectionDescriptor = (dbConnectionDescriptors.length==0) ? application.createMainDbConnectionDescriptor() : dbConnectionDescriptors[0];										
		new DbConnectionDescriptorDialog(mainDbConnectionDescriptor);	
	});
}

MainMenu.prototype.getWorkflow = function(){
	return this.workflow;
}

MainMenu.prototype.addSeparator = function(){
	$('#'+this.mainMenuContainerId+' ul').append('<li class="mainmenu-separator"></li>');
}

MainMenu.prototype.addButton = function(/*:String*/ tooltip, /*:Int*/ icon, /*:Function*/ callback){

	if ( (!(callback)) || typeof(callback) != 'function' )
		callback = function(){};

	var icons = new Array(
		'ui-icon-folder-open',
		'ui-icon-disk',
		'ui-icon-clipboard',
		'ui-icon-grip-dotted-vertical',
		'ui-icon-triangle-1-sw',
		'ui-icon-triangle-1-ne',
		'ui-icon-newwin',
		'ui-icon-transferthick-e-w'
	);

	return $('<li class="ui-state-default ui-corner-all" title="'+tooltip+'"><span class="ui-icon '+icons[icon]+'"></span></li>')
		.appendTo('#'+this.mainMenuContainerId+' ul')
		.bind('click',callback)
		.hover(
			function() { $(this).addClass('ui-state-hover'); },
			function() { $(this).removeClass('ui-state-hover'); }
		)
		.tooltip({
			track: true,
			bodyHandler: function() { return tooltip; },
			delay: 0
		});
}