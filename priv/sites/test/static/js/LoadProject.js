XMLImport = function(){}


/** @static */
XMLImport.list = function(){
	
	var dialog = null;
			
	$.ajax({
		url: "list.jsp",
		dataType: "html",
		beforeSend: function (request) {
			dialog = new ConfirmDialog( 'Open project', 'Please wait...');
		},
		success: function(data, textStatus){
			var tagname = ($(data).attr("tagName")) ? $(data).attr("tagName").toLowerCase() : 'undefined';
			
			if (tagname=="select"){
				dialog.show( 'Open project', 'Select a file to load:<br/>'+data, function(){ XMLImport.getfile($(this).find('select').val()); } );
			}else{
				dialog.show( 'Open project', data );
			}
		},
		error: function (request, textStatus, errorThrown) {
			dialog.show( 'Error', '<strong>Unable to get projects list.</strong><br/>Ajax error: '+textStatus);
		}
	});	
	
}

/** @private @static */
XMLImport.currentProject = null;


/** @private @static */
XMLImport.getfile = function(fileToLoad){

	var dialog = null;

	$.ajax({
		url: "load.jsp",
		type: "POST",
		processData: true,
		data: { project_name: fileToLoad },
		dataType: "text",
		beforeSend: function (request) {
			dialog = new ConfirmDialog( 'Open project', 'Please wait...');
		},
		success: function(data, textStatus){
			
			data = $.trim(data);
			
			if (data.length > 30 && data.substring(0,5).toLowerCase()=='<?xml'){
				dialog.dispose();
				dialog = null;
				
				var parsing_error = false;
				
				var $xml = $.xmlDOM(
					data, 
					function(error) { 
						dialog.show( 'Error', '<strong>Unable to parse xml:</strong> ' + error);  
						parsing_error=true; 
					}
				);
				
				if (parsing_error==true)
					return false;
				
				XMLImport.currentProject = null;
				if (XMLImport.execute($xml)==true){
					XMLImport.currentProject = fileToLoad;
				}
				
			}else
				dialog.show( 'Error', 'Received data is corrupted, too small, or bad formed.' );
		},
		error: function (request, textStatus, errorThrown) {
			dialog.show( 'Error', '<strong>Unable to get file content.</strong><br/>Ajax error: '+textStatus);
		}
	});
	
}


/** @private @static */
XMLImport.execute = function($xml){

	main();
	var workflow = application.getWorkflow();

	//---- function --------------------------
	var addPage = function(tag){
		var pagefig = new PageElement();
		var page = new Page(pagefig);
		
		page.name = $(tag).attr('alias');
		page.url = $(tag).attr('url');
		page.setId( $(tag).attr('id') );
		
		workflow.addFigure(pagefig,new Number($(tag).attr('FigX')),new Number($(tag).attr('FigY')));
		
		pagefig.model = page;
		pagefig.setDimension($(tag).attr('FigWidth'), $(tag).attr('FigHeight'));
		//pagefig.setZOrder($(tag).attr('FigZOrder'));
		pagefig.setTitle(pagefig.getModel().name);
		
		return pagefig;
	}

    var addTemplate = function(tag){
        var pagefig = new TemplateElement();
        var page = new Template(pagefig);

        page.name = $(tag).attr('alias');
        page.url = $(tag).attr('url');
        page.setId( $(tag).attr('id') );

        workflow.addFigure(pagefig,new Number($(tag).attr('FigX')),new Number($(tag).attr('FigY')));

        pagefig.model = page;
        pagefig.setDimension($(tag).attr('FigWidth'), $(tag).attr('FigHeight'));
        //pagefig.setZOrder($(tag).attr('FigZOrder'));
        pagefig.setTitle(pagefig.getModel().name);

        return pagefig;
    }


    var addResource = function(tag){
        var pagefig = new ResourceElement();
        var page = new Resource(pagefig);

        page.name = $(tag).attr('alias');
        page.url = $(tag).attr('url');
        page.setId( $(tag).attr('id') );

        workflow.addFigure(pagefig,new Number($(tag).attr('FigX')),new Number($(tag).attr('FigY')));

        pagefig.model = page;
        pagefig.setDimension($(tag).attr('FigWidth'), $(tag).attr('FigHeight'));
        //pagefig.setZOrder($(tag).attr('FigZOrder'));
        pagefig.setTitle(pagefig.getModel().name);

        return pagefig;
    }

    var addDataStore = function(tag){
        var pagefig = new DataStoreElement();
        var page = new DataStore(pagefig);

        page.name = $(tag).attr('alias');
        page.url = $(tag).attr('url');
        page.setId( $(tag).attr('id') );

        workflow.addFigure(pagefig,new Number($(tag).attr('FigX')),new Number($(tag).attr('FigY')));

        pagefig.model = page;
        pagefig.setDimension($(tag).attr('FigWidth'), $(tag).attr('FigHeight'));
        //pagefig.setZOrder($(tag).attr('FigZOrder'));
        pagefig.setTitle(pagefig.getModel().name);

        return pagefig;
    }
	
	//---- function --------------------------
	var addState = function(tag, pagefig){
		var statefig = new StateElement();
		var state = new State(statefig);
		
		state.name = $(tag).attr('name');
		state.initial = (new String($(tag).attr('initial')).toLowerCase()=="true") ? true : false;
		state.setId( $(tag).attr('id') );
		state.page = pagefig.getModel();
		
		
		workflow.addFigure(statefig,new Number($(tag).attr('FigX')),new Number($(tag).attr('FigY')));

		statefig.model = state;
		pagefig.addChild(statefig);
		pagefig.getModel().addChild(state);
		//statefig.getModel().setParent(pagefig.getModel());
		
		statefig.setTitle(statefig.getModel().name);

		return statefig;
	}
	
	//---- function --------------------------
	var addTransition = function(tag, transaction){
		
		var sourcefig = application.getElementById( $(tag).attr("sourceStateId") ).getParent();
		var targetfig = application.getElementById( $(tag).attr("targetStateId") ).getParent();
        
		var transitionfig = new TransitionElement(sourcefig.portSn,targetfig.portSn);
		transitionfig.setSource(sourcefig.portSn);
		transitionfig.setTarget(  (sourcefig.getModel().getId()==targetfig.getModel().getId() ) ? sourcefig.portDx : targetfig.portSn );

		workflow.addFigure(transitionfig);
		
		var transition = new Transition( transitionfig, sourcefig.getModel(), targetfig.getModel() );
	    transition.name = $(tag).attr("name");
		transition.source = sourcefig.getModel();
		transition.target = targetfig.getModel();
		transition.transaction = transaction;
		transition.setId($(tag).attr("id"));
		
		transitionfig.model = transition;
		transitionfig.setTitle(transition.name);
				
				
		$(tag).find('TriggerEvent').each(function(){
			addTriggerEvent(this,transition);
		});
		
		$(tag).find('Action').each(function(){
			addAction(this,transition);
		});
		
		return transition;
	}
	
	//---- function --------------------------
	var addTransaction = function(tag){
		var t = new Transaction($(tag).attr("name"));
		t.setId($(tag).attr("id"))
		
		application.nonGraphicalElements.push( t );
		return t;
	}
	
	//---- function --------------------------
	var addDbConnectionDescriptor = function(tag){
		var dbConnectionDescriptor = new DbConnectionDescriptor($(tag).attr("driver"),$(tag).attr("url"),$(tag).attr("user"),$(tag).attr("pass"));
		dbConnectionDescriptor.setId($(tag).attr("id"));
		
		application.nonGraphicalElements.push( dbConnectionDescriptor );
		return dbConnectionDescriptor;
	}	
	
	//---- function --------------------------
	var addGraphicalElement = function(tag, pagefig){
		var ge = new GraphicalElement(pagefig.getModel(),$(tag).attr("name"),$(tag).attr("type"));
		ge.setId($(tag).attr("id"));
		pagefig.getModel().addChild(ge);
		ge.setParent(pagefig.getModel());
		
		return ge;
	}
	
	//---- function --------------------------
	var addStateParameter = function(tag, statefig){
		var sp = new StateParameter(statefig.getModel(),$(tag).attr("name"),$(tag).attr("type"));
		sp.setId($(tag).attr("id"));
		statefig.getModel().addChild(sp);
		sp.setParent(statefig.getModel());
		
		return sp;
	}
	
	//---- function --------------------------
	var addTriggerEvent = function(tag, transition){
		var te = new TriggerEvent( application.getElementById($(tag).attr("graphicalElementId")), $(tag).attr("eventType") );
		te.setId( $(tag).attr("id") );
		
		transition.addChild(te);
		te.setParent(transition);
		
		return te;
	}
	
	//---- function --------------------------
	var addAction = function(tag, transition){
		var actionType = $(tag).attr("type");
		var parameters = new Array();
		var action = null;
		
		$(tag).find('ActionParameter').each(function(){
			var sourceObj = application.getElementById($(this).attr("sourceId"));
			var acPar = new ActionParameter( $(this).attr("name"), sourceObj, $(this).attr("type"));
			//alert(acPar);
			parameters.push(acPar);
		});
			
		if (actionType == "model.action.simple_db_insert")
			action = new SimpleDbInsertAction(parameters);
		else if (actionType == "model.action.simple_db_delete")
			action = new SimpleDbDeleteAction(parameters);
		else if (actionType == "model.action.simple_db_oneway_delete")
			action = new SimpleDbOnewayDeleteAction(parameters);
		else if (actionType == "model.action.simple_db_oneway_insert")
			action = new SimpleDbOnewayInsertAction(parameters);
		else{}
		
		if (action!=null){
			action.setId($(tag).attr("id"));
			transition.addChild(action);
			action.setParent(transition);
		}

		return action;
	}
	
	//main.....
	$xml.find('Page').each(function(){
		var pagefig = addPage(this);
			$(this).find('State').each(function(){
				var statefig = addState(this, pagefig);
				$(this).find('StateParameter').each(function(){
					addStateParameter(this,statefig);
				});
			});
			$(this).find('GraphicalElement').each(function(){
				addGraphicalElement(this, pagefig);
			});
	});


	$xml.find('Transitions').each(function(){
		$(this).find('Transition').each(function(){
			addTransition(this,null);
		});
	});

	$xml.find('Transaction').each(function(){
		var transaction = addTransaction(this);
		$(this).find('Transition').each(function(){
			addTransition(this,transaction);
		});
	});

	$xml.find('DbConnectionDescriptor').each(function(){
		addDbConnectionDescriptor(this);
	});

	return true;
}