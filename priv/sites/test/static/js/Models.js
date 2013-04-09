/*------------------------------------------------------------------------------------------------------------------
	Application Class
------------------------------------------------------------------------------------------------------------------*/
Application = function( /*:draw2d.Workflow*/ workflow ){
	this.workflow = workflow;
	this.nonGraphicalElements = new Array();
}

Application.prototype.getWorkflow = function(){
	return this.workflow;
}

Application.prototype.clear = function(){
	this.workflow.clear();
	this.nonGraphicalElements = new Array();
	elementsListDialog.refresh();
}

Application.prototype.generateUID = function(){
	var id = null;
	var elements = this.getElements();

	while( true ){
		id = 'E' + Math.floor(Math.random()*100000);
		var unique = true;
		$(elements).each( function(i,e){
			if ( e.getId() == id ){
				unique = false;
				return false; /*equivale a break. Vedere Jquery Reference per la funzione $.each*/
			}
		});
		if ( unique==true )
			break;
	}
	return id;
}


Application.prototype.getElements = function(){
	var founded = new Array();
	
	/*var addSecure = function (m){
		if (founded.indexOf(m)>=0)
			return;	
		founded.push(m);
	}*/
	
	var addAllModelChildren = function(m){
		//addSecure( m );
		founded.push(m);
		var x=0;
		for (x=0; x<m.getChildren().length; x++)
			addAllModelChildren( m.getChildren()[x] );
	}

	var figures = this.workflow.getDocument().getFigures();

	for(var i=0;i< figures.getSize(); i++){

		var figure = figures.get(i);

		if ( figure.getModel &&  figure.getModel()!=null ){
			addAllModelChildren( figure.getModel() );
		}

	}

	var lines = this.workflow.getDocument().getLines();

	for(var i=0;i< lines.getSize(); i++){

		var line = lines.get(i);

		if ( line.getModel && line.getModel()!=null )
			addAllModelChildren( line.getModel() );
	}
	
	for(var i=0; i<this.nonGraphicalElements.length; i++){
		founded.push( this.nonGraphicalElements[i] );
	}
	
	return founded;
}


Application.prototype.getAvailableName = function( /*:String*/ basename, obj ){
	var brothers = this.getElementsByType ( obj.getType() );
	var number_to_append = 0;
	var oThis = this;

	var getNumberAtTheEndOfString = function ( /*:String*/ str ){
		var number = '';
		var exists = false;
		for (i=0; i<str.length; i++){
			var c = parseInt (str[i]);
			if ( isFinite(c) ){
				number += c;
				exists = true;
			}
		}
		if (exists == false)
			return 0
		else
			return number;
	}

	$(brothers).each( function(i,e){
		temp = parseInt( getNumberAtTheEndOfString ( e.name ) );
		if ( temp > number_to_append)
			number_to_append = temp;
	});
	return ( basename + '' + (number_to_append+1) );
}


/**
	@deprecated ONLY use this function in a way like application.getElementsByType( o.getType() ) instead of writing your own string type for example application.getElementsByType('elem.type1')
*/
Application.prototype.getElementsByType = function (/*:String*/ type){
	var founded = new Array();

	var elements = this.getElements();
	for (i=0; i<elements.length; i++)
		if ( elements[i].getType() == type )
			founded.push( elements[i] );

	return founded;
}

Application.prototype.getElementsByClassName = function (/*:Object*/ _className){
	var founded = new Array();
	var elements = this.getElements();
	for (i=0; i<elements.length; i++)
		if ( elements[i] instanceof _className )
			founded.push( elements[i] );
	return founded;
}

Application.prototype.getElementById = function (/*:Object*/ id){
	var elements = this.getElements();
	for (i=0; i<elements.length; i++)
		if ( elements[i].getId() == id )
			return elements[i];

	return null;
}

Application.prototype.createMainDbConnectionDescriptor = function(){

	var mainDbConnectionDescriptor = new DbConnectionDescriptor(DbConnectionDescriptor.defaultDriver, DbConnectionDescriptor.defaultUrl, DbConnectionDescriptor.defaultUser, DbConnectionDescriptor.defaultPass);
	this.nonGraphicalElements.push( mainDbConnectionDescriptor );	
	
	return mainDbConnectionDescriptor;
}

/*------------------------------------------------------------------------------------------------------------------
	Generic Model Class
------------------------------------------------------------------------------------------------------------------*/
GenericModel = function(){
	this.type = 'model';
	this.parent = null;
	this.propertyList = new Array();
	this.id = ((typeof application) == 'undefined') ? -1 : application.generateUID();
	this.children = new Array();
}

GenericModel.prototype.getType = function(){ return this.type; }
GenericModel.prototype.setType = function( type ){ this.type=type };

GenericModel.prototype.getParent = function(){ return this.parent; }
GenericModel.prototype.setParent = function( parent ){ this.parent=parent };

GenericModel.prototype.getId = function(){ return this.id; }
GenericModel.prototype.setId = function( id ){ this.id=id };

GenericModel.prototype.getPropertyList = function(){ return this.propertyList; }
GenericModel.prototype.setPropertyList = function( propertyList ){ this.propertyList=propertyList };

GenericModel.prototype.getChildren = function(){ return this.children; }
GenericModel.prototype.addChild = function( obj ){ this.children.push( obj ); }
GenericModel.prototype.getChildrenByClassName = function ( className ){
	var founded = new Array();
	$.each( this.children, function(i,el){
		if ( el instanceof className )
			founded.push( el );
	});
	return founded;
}
GenericModel.prototype.removeChildren = function(){ this.children = new Array() }
GenericModel.prototype.removeChild = function( obj ){ this.children.splice(this.children.indexOf(obj),1) }

/** @override */
GenericModel.prototype.toString = function(){
	var name = '?';

	if ( this instanceof State) name = this.name;
	else if ( this instanceof StateParameter) name = this.param_name;
	else if ( this instanceof Page) name = this.name;
	else if ( this instanceof Transition) name = this.name;
	else if ( this instanceof GraphicalElement) name = this.graphelem_name;
	else if ( this.name ) name = this.name;
	else name = '<Obj name(?)>';

	return name + ' [' + this.getType() + ']';
}


/*------------------------------------------------------------------------------------------------------------------
	(Generic) Property Class
------------------------------------------------------------------------------------------------------------------*/
Property = function(name, description, editable, property_type, getCallback, setCallback, relatedClass){
	this.name = name;
	this.description = description;
	this.editable = editable;
	this.property_type = property_type;
	this.relatedClass  = (relatedClass ) ? relatedClass  : null;
	//Free to expand..

	/** @private */
	this.getCallback = getCallback;

	/** @private */
	this.setCallback = setCallback;
}
Property.prototype.get = function(){ return this.getCallback(); }
Property.prototype.set = function( value ){ this.setCallback(value); }


/*------------------------------------------------------------------------------------------------------------------
	State Parameter Class
------------------------------------------------------------------------------------------------------------------*/
StateParameter = function(/*:State*/ state, param_name, param_type ){
	GenericModel.call(this);

	var oThis = this;
	this.setType( this.getType() + ".state_parameter" );
	this.state = state
	this.param_type = param_type;
	this.param_name = param_name;

	this.setPropertyList ( new Array(
		new Property('Name', '?', true, 'text', function(){ return oThis.param_name }, function(value){ oThis.param_name = value } ),
		new Property('Type', '?', false, 'select', function(){ return oThis.param_type }, function(value){ oThis.param_type = value } )
	));
}

StateParameter.prototype = new GenericModel;

/** @static */
StateParameter.types = new Array(
	'String',
	'Numeric',
	'Object'
	//Free to implement
);


/*------------------------------------------------------------------------------------------------------------------
	Graphical element (Class)
------------------------------------------------------------------------------------------------------------------*/
GraphicalElement = function(/*:Page*/ page, graphelem_name, graphelem_type ){
	GenericModel.call(this);

	var oThis = this;
	this.setType( this.getType() + ".graphical_element" );
	this.page = page
	this.graphelem_name = graphelem_name;
	this.graphelem_type = graphelem_type;

	this.setPropertyList ( new Array(
		new Property('Name', '?', true, 'text', function(){ return oThis.graphelem_name }, function(value){ oThis.graphelem_name = value } ),
		new Property('Type', '?', false, 'select', function(){ return oThis.graphelem_type }, function(value){ oThis.graphelem_type = value } )
	));

}

GraphicalElement.prototype = new GenericModel;

/** @static */
GraphicalElement.events = new Array(
	'onClick',
	'onDblClick',
	'onChange',
	'onBlur',
	'onFocus',
	'onMouseDown',
	'onMouseMove',
	'onMouseOver',
	'onMouseOut',
	'onMouseUp',
	'onKeydown',
	'onKeypress',
	'onKeyup',
	'onSelect',
	'onSubmit'
	//Free to implement
);

/** @static */
GraphicalElement.types = new Array(
	'Button',
	'Input[text]',
	'Input[radio]',
	'Input[checkbox]',
	'Input[reset]',
	'Input[password]',
	'Input[hidden]'
	//Free to implement
);

/*------------------------------------------------------------------------------------------------------------------
	State Class
------------------------------------------------------------------------------------------------------------------*/
State = function(/*draw2d.figure*/ parent){

	GenericModel.call(this);

	var oThis = this; 
	this.setParent( parent );
	this.setType( this.getType() + ".state" );
	this.name = application.getAvailableName( 'State', this );
	this.page = null;
	this.initial = false;

	this.setPropertyList ( new Array(
		new Property('Name', '?', true, 'text', function(){ return oThis.name }, function(value){ oThis.name = value; oThis.getParent().setTitle(value); } ),
		new Property('Page', '?', false, 'text', function(){ return (oThis.page!=null) ? oThis.page.name : 'NULL' }, function(value){ oThis.page = value } ),
		new Property('Initial', '?', true, 'bool', function(){ return oThis.initial }, function(value){ oThis.initial = value } ),
		new Property('Parameters', '?', false, 'array', function(){ return oThis.getParameters() }, function(value){ }, StateParameter )
	));

}

State.prototype = new GenericModel;

State.prototype.getParameters = function(){ return this.getChildrenByClassName( StateParameter ); }

State.prototype.setInitial = function(){
	$(application.getElementsByClassName(State)).each(function(){ this.initial = false; });
	this.initial = true;
}
/*------------------------------------------------------------------------------------------------------------------
	Page Class
------------------------------------------------------------------------------------------------------------------*/
Page=function(/*draw2d.figure*/ parent){

    GenericModel.call(this);

    var oThis = this;
    this.setParent( parent );
    this.setType( this.getType() + ".page" );
    this.name = application.getAvailableName( 'Page', this );
    this.url = '/'+this.name+'.jsp';


    this.setPropertyList ( new Array(
        new Property('Name', '?', true, 'text', function(){ return oThis.name }, function(value){ oThis.name = value; oThis.getParent().setTitle(value); } ),
        new Property('Url', '?', true, 'text', function(){ return oThis.url }, function(value){ oThis.url = value; } ),
        new Property('Graphical Elements', '?', false, 'array', function(){ return oThis.getChildrenByClassName( GraphicalElement ) }, function(value){}, GraphicalElement )
    ));

}

Page.prototype = new GenericModel;

Page.prototype.getGraphicalElements = function(){ return this.getChildrenByClassName( GraphicalElement ) }
Page.prototype.getStates = function(){ return this.getChildrenByClassName( State ) }

/*------------------------------------------------------------------------------------------------------------------
 Resource Class
 ------------------------------------------------------------------------------------------------------------------*/
Resource=function(/*draw2d.figure*/ parent){

    GenericModel.call(this);

    var oThis = this;
    this.setParent( parent );
    this.setType( this.getType() + ".resource" );
    this.name = application.getAvailableName( 'Resource', this );
    this.url = '/'+this.name+'.jsp';


    this.setPropertyList ( new Array(
        new Property('Name', '?', true, 'text', function(){ return oThis.name }, function(value){ oThis.name = value; oThis.getParent().setTitle(value); } ),
        new Property('Url', '?', true, 'text', function(){ return oThis.url }, function(value){ oThis.url = value; } ),
        new Property('Graphical Elements', '?', false, 'array', function(){ return oThis.getChildrenByClassName( GraphicalElement ) }, function(value){}, GraphicalElement )
    ));

}

Resource.prototype = new GenericModel;

Resource.prototype.getGraphicalElements = function(){ return this.getChildrenByClassName( GraphicalElement ) }
Resource.prototype.getStates = function(){ return this.getChildrenByClassName( State ) }

/*------------------------------------------------------------------------------------------------------------------
    Resource Class
------------------------------------------------------------------------------------------------------------------*/
Template=function(/*draw2d.figure*/ parent){

    GenericModel.call(this);

    var oThis = this;
    this.setParent( parent );
    this.setType( this.getType() + ".template" );
    this.name = application.getAvailableName( 'Template', this );
    this.url = '/'+this.name+'.jsp';


    this.setPropertyList ( new Array(
        new Property('Name', '?', true, 'text', function(){ return oThis.name }, function(value){ oThis.name = value; oThis.getParent().setTitle(value); } ),
        new Property('Url', '?', true, 'text', function(){ return oThis.url }, function(value){ oThis.url = value; } ),
        new Property('Graphical Elements', '?', false, 'array', function(){ return oThis.getChildrenByClassName( GraphicalElement ) }, function(value){}, GraphicalElement )
    ));

}

Template.prototype = new GenericModel;

Template.prototype.getGraphicalElements = function(){ return this.getChildrenByClassName( GraphicalElement ) }
Template.prototype.getStates = function(){ return this.getChildrenByClassName( State ) }


/*------------------------------------------------------------------------------------------------------------------
 Resource Class
 ------------------------------------------------------------------------------------------------------------------*/
DataStore=function(/*draw2d.figure*/ parent){

    GenericModel.call(this);

    var oThis = this;
    this.setParent( parent );
    this.setType( this.getType() + ".datastore" );
    this.name = application.getAvailableName( 'DataStore', this );
    this.url = '/'+this.name+'.jsp';


    this.setPropertyList ( new Array(
        new Property('Name', '?', true, 'text', function(){ return oThis.name }, function(value){ oThis.name = value; oThis.getParent().setTitle(value); } ),
        new Property('Url', '?', true, 'text', function(){ return oThis.url }, function(value){ oThis.url = value; } ),
        new Property('Graphical Elements', '?', false, 'array', function(){ return oThis.getChildrenByClassName( GraphicalElement ) }, function(value){}, GraphicalElement )
    ));

}

DataStore.prototype = new GenericModel;

DataStore.prototype.getGraphicalElements = function(){ return this.getChildrenByClassName( GraphicalElement ) }
DataStore.prototype.getStates = function(){ return this.getChildrenByClassName( State ) }



/*------------------------------------------------------------------------------------------------------------------
	Transition Class
------------------------------------------------------------------------------------------------------------------*/
Transition=function(/*draw2d.figure*/ parent, /*:State*/source, /*:State*/target){

	GenericModel.call(this);

	var oThis = this;
	this.setParent( parent );
	this.setType( this.getType() + ".transition" );
	this.name = application.getAvailableName( 'Transition', this );
	this.source = source;
	this.target = target;
	this.transaction = null;

	this.setPropertyList ( new Array(
		new Property('Name', '?', true, 'text', function(){ return oThis.name }, function(value){ oThis.name = value; oThis.getParent().setTitle(value); } ),
		new Property('Source', '?', false, 'text', function(){ return oThis.source.name }, function(value){} ),
		new Property('Target', '?', false, 'text', function(){ return oThis.target.name }, function(value){} ),
		new Property('Triggered Actions', '?', false, 'array', function(){ return oThis.getTriggeredActions()  }, function(value){}, Action ),
		new Property('Trigger Events', '?', false, 'array', function(){ return oThis.getTriggerEvents()  }, function(value){}, TriggerEvent ),
		new Property('Transaction', '?', true, 'select', function(){ return oThis.transaction }, function(trnac_id){ oThis.transaction = application.getElementById(trnac_id); oThis.getParent().setTitle(oThis.name); } )
	));

}
Transition.prototype = new GenericModel;

Transition.prototype.getTriggeredActions = function(){ return this.getChildrenByClassName( Action ); }
Transition.prototype.getTriggerEvents = function(){ return this.getChildrenByClassName( TriggerEvent ); }


/*------------------------------------------------------------------------------------------------------------------
	Transaction Class
------------------------------------------------------------------------------------------------------------------*/
Transaction=function(/*:String*/ name){

	GenericModel.call(this);

	var oThis = this;
	this.setType( this.getType() + ".transaction" );
	this.name = (name) ? name : application.getAvailableName( 'Transaction', this );

	this.setPropertyList ( new Array(
		new Property('Name', '?', true, 'text', function(){ return oThis.name }, function(value){ oThis.name = value } )
	));

}
Transaction.prototype = new GenericModel;

/*------------------------------------------------------------------------------------------------------------------
	DbConnectionDescriptor Class
------------------------------------------------------------------------------------------------------------------*/
DbConnectionDescriptor = function(driver,url,user,pass){
	GenericModel.call(this);

	var oThis = this;
	this.setType( this.getType() + ".DbConnectionDescriptor" );
	
	this.driver = driver;
	this.url = url;
	this.user = user;
	this.pass = pass;
}
DbConnectionDescriptor.prototype = new GenericModel;

DbConnectionDescriptor.defaultDriver = 'com.mysql.jdbc.Driver';
DbConnectionDescriptor.defaultUrl = 'jdbc:mysql://localhost/testsitedb';
DbConnectionDescriptor.defaultUser = 'root';
DbConnectionDescriptor.defaultPass = 'root';

/*------------------------------------------------------------------------------------------------------------------
	Action Class
------------------------------------------------------------------------------------------------------------------*/
Action = function(){
	GenericModel.call(this);

	var oThis = this;
	this.setType( this.getType() + ".action" );
	this.parameters = null;
	
	this.setPropertyList ( new Array(
		//new Property('Params count', '?', false, 'text', function(){ return oThis.parameters.length; }, function(value){} )
	));
}
Action.prototype = new GenericModel;

Action.prototype.getParameters = function(){ return this.parameters; }


/*-----------------------------------------------------------------------------------------------------------------*/
ActionParameter = function(name, source, type){
	this.type = (type) ? type : null;
	this.name = (name) ? name : null;
	this.source = (source) ? source : null;
}

ActionParameter.prototype.toString = function(){
	return "ActionParameter: "+this.name+", "+this.type+", "+this.source;
}

ActionParameter.tableName = "TableName";

/*-----------------------------------------------------------------------------------------------------------------*/
SimpleDbInsertAction = function(parameters){
	Action.call(this);
	
	this.parameters = ((parameters) && (parameters instanceof Array)) ? parameters : (new Array());
	this.setType( this.getType() + ".simple_db_insert" );
}
SimpleDbInsertAction.prototype = new Action;

/*-----------------------------------------------------------------------------------------------------------------*/
SimpleDbDeleteAction = function(parameters){
	Action.call(this);
	
	this.parameters = ((parameters) && (parameters instanceof Array)) ? parameters : (new Array());
	this.setType( this.getType() + ".simple_db_delete" );
}
SimpleDbDeleteAction.prototype = new Action;

/*-----------------------------------------------------------------------------------------------------------------*/
SimpleDbOnewayInsertAction = function(parameters){
	Action.call(this);
	
	this.parameters = ((parameters) && (parameters instanceof Array)) ? parameters : (new Array());
	this.setType( this.getType() + ".simple_db_oneway_insert" );
}
SimpleDbOnewayInsertAction.prototype = new Action;

/*-----------------------------------------------------------------------------------------------------------------*/
SimpleDbOnewayDeleteAction = function(parameters){
	Action.call(this);
	
	this.parameters = ((parameters) && (parameters instanceof Array)) ? parameters : (new Array());
	this.setType( this.getType() + ".simple_db_oneway_delete" );
}
SimpleDbOnewayDeleteAction.prototype = new Action;



/*------------------------------------------------------------------------------------------------------------------
	TriggerEvent Class
------------------------------------------------------------------------------------------------------------------*/
TriggerEvent = function( graph_elem, event_type ){
	GenericModel.call(this);

	var oThis = this;
	this.setType( this.getType() + ".trigger_event" );

	this.graph_elem = graph_elem;
	this.event_type = event_type;

	this.setPropertyList( new Array(
		new Property('Graphical element', '?', false, 'text', function(){ return oThis.graph_elem.graphelem_name + ' (' + oThis.graph_elem.page.name + ')' }, function(value){oThis.graph_elem = value} ),
		new Property('Event type', '?', true, 'select', function(){ return oThis.event_type }, function(value){oThis.event_type = value} )
	));

}
TriggerEvent.prototype = new GenericModel;

