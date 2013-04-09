$(window).bind('load',function(){
	main();
});



/**
	Load everything.
	If this function is called when the editor as been already loaded 
	workflow is cleaned and its callback functions are restored.
*/
function main(){
	
	var reinitialize_workflow = function (_workflow){
	
		_workflow.dialogs.add( window.paletteDialog );
		_workflow.getCommandStack().addCommandStackEventListener( window.commandListener );
		_workflow.addSelectionListener( window.propertyDialog );
	
	}
	
	if (!(window.application)){

		var finalize_loading = function(){
			$loading.hide();	
			
			XMLImport.currentProject = null;
			
			$header.append('<ul><li><a style="display:none" href="'+window.about_link+'" >About this project</a></li></ul><img src="'+window.imgs_path+'logo.png"/><h1>Modelling Safe Interface Interactions in Web Applications</h1><h2>Application\'s static structure editor ('+window.editor_version+')</h2>');
			$('body').append('<div id="paintarea-wrapper"><div id="paintarea" class="unselectable"></div></div>');	
			
			
			var workflow = new draw2d.Workflow("paintarea");
			//workflow.setEnableSmoothFigureHandling(true);
			
			window.application = new Application(workflow);
			window.paletteDialog = new PaletteDialog(workflow);
			window.propertyDialog = new PropertyDialog(workflow);
			window.mainMenu = new MainMenu(workflow);
			window.elementsListDialog = new ElementsListDialog(workflow);
			window.commandListener = new CommandListener(workflow);	
			
			reinitialize_workflow(workflow);
		}
		
		
		
		var scripts = new Array(
			'<script type="text/javascript" src="js/wz_jsgraphics.js"></script>',
			'<script type="text/javascript" src="js/draw2d.js"></script>',
			'<script type="text/javascript" src="js/jquery-ui-1.7.2.custom.min.js"></script>',
			'<script type="text/javascript" src="js/jquery.tooltip.min.js"></script>',
			'<script type="text/javascript" src="js/jquery-ui-accordion-1.8.milestone.js"></script>', 
			'<script type="text/javascript" src="js/jquery.xmldom-1.0.min.js"></script>',
			'<script type="text/javascript" src="js/StateElement.class.js"></script>',
			'<script type="text/javascript" src="js/PageElement.class.js"></script>',
			'<script type="text/javascript" src="js/PropertyDialog.class.js"></script>',
			'<script type="text/javascript" src="js/PaletteDialog.class.js"></script>',
			'<script type="text/javascript" src="js/Dialogs.js"></script>',
			'<script type="text/javascript" src="js/MainMenu.class.js"></script>',
			'<script type="text/javascript" src="js/ElementsListDialog.class.js"></script>',
			'<script type="text/javascript" src="js/StateElementPort.class.js"></script>',
			'<script type="text/javascript" src="js/CommandListener.class.js"></script>',
			'<script type="text/javascript" src="js/CommandChangeModelProperty.class.js"></script>',
			'<script type="text/javascript" src="js/CommandAddModelChild.class.js"></script>',
			'<script type="text/javascript" src="js/CommandRemoveModelChild.class.js"></script>',
			'<script type="text/javascript" src="js/TransitionLabelLocator.class.js"></script>',
			'<script type="text/javascript" src="js/CommandDeleteState.class.js"></script>',
			'<script type="text/javascript" src="js/CommandAddState.class.js"></script>',
			'<script type="text/javascript" src="js/CommandDeletePage.class.js"></script>',
			'<script type="text/javascript" src="js/Models.js"></script>',
			'<script type="text/javascript" src="js/TransitionElement.class.js"></script>',
			'<script type="text/javascript" src="js/TransitionRouter.class.js"></script>',
			'<script type="text/javascript" src="js/SaveProject.js"></script>',
			'<script type="text/javascript" src="js/LoadProject.js"></script>',
			'<script type="text/javascript" src="js/Misc.js"></script>'
		);
		
		
		var loadscript = function(m){
			if (m==scripts.length){
				finalize_loading();
				return;
			}
			else{
				$.getScript($(scripts[m]).attr('src'), function(){
					$loading.text( 'Loading [' + (parseInt((m*100)/(scripts.length-1)))+ '%]' );
					setTimeout( function(){loadscript(m+1)} ,10);
				});
			}
		}
		
		
		window.imgs_path = "img/"; //with final slash
		window.editor_version = 'v.0.8.5 Beta';
		window.about_link = '../';

		$.ajaxSetup({async: false});
		
		var $header = $('<div id="header"><div id="loading_progress">Loading</div></div>').appendTo('body').fadeIn();
			$header.before('<div id="preheader"></div>');
		
		var $loading = $header.find('#loading_progress');			
		
		
		var loaded_without_apache = false;
		$('head script').each(function(){ loaded_without_apache = ($(this).attr('src')=='js/Models.js') ? true : false; if (loaded_without_apache==true){return false;} });
		
		
		if (loaded_without_apache==true)
			finalize_loading();
		else
			loadscript(0);	
			
		

	}else{
		window.application.clear();
		reinitialize_workflow( window.application.getWorkflow() );
	}
}