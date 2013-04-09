//Override the addChild method => now only State can be added as a child
(function(){
	var old_func = draw2d.CompartmentFigure.prototype.addChild;
	draw2d.CompartmentFigure.prototype.addChild = function(figure){
		if ( !(figure.getModel) | figure.getModel()==null | !(figure.getModel() instanceof State) )
			return;
		old_func.call(this,figure);
	}
})();




//Extend the javascript String object adding the isClean function
String.prototype.isClean = function(){
	var allowed = new Array('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','0','1','2','3','4','5','6','7','8','9','_','-','.',' ','(',')','[',']');
	var temp = this.toLowerCase();
	var valid = false;
	for (i=0; i<temp.length; i++){
		valid = false;
		for(k=0; k<allowed.length; k++){
			if ( temp[i] == allowed[k] ){
				valid = true; break;
			}
		}
		if (valid==false)
			return false;
	}
	return true;
};
	
	
/**
	Checks if a graphical element that is binded to a trigger event.
	Used on deleting a graphical element.
*/
function isBindedToTriggerEvents(/*:GraphicalElement*/ ge, /*:bool*/ show_msg_dialog){
	var result = false;
	
	if (!(ge instanceof GraphicalElement))
		return false;
	
	var bindedTriggerEvents = new Array();
	$(application.getElementsByClassName(TriggerEvent)).each(function(){
		if (this.graph_elem == ge)
			bindedTriggerEvents.push(this);
	});
	if (bindedTriggerEvents.length!=0){
		result = true;
		
		if (show_msg_dialog==true){
			var html="This graphical element is binded to the following trigger events:<ul>";
					$(bindedTriggerEvents).each(function(){ html+="<li>"+this.getId()+" that trigger <i>"+this.getParent().name+"</i></li>"; });
					html+="</ul>";

			MessageDialog.show('Error',html,"error-icon");
		}
	}
	

	return result;
}	


/**
	Checks if a graphical element or statepa rameter that is binded to an action.
	Used on deleting a graphical element or a stateparameter.
*/
function isBindedToAction(elem, /*:bool*/ show_msg_dialog){
	var result = false;
	var bindedActions = new Array();
		
	$(application.getElementsByClassName(Action)).each(function(i,action){
		$(this.getParameters()).each(function(k,actionParameter){
			if (actionParameter.source == elem)
				bindedActions.push( action );
		});
	});
	
	if (bindedActions.length!=0){
		result = true;
		
		if (show_msg_dialog==true){
			var html="This element is binded to an action.";
			MessageDialog.show('Error',html,"error-icon");
		}
	}
	return result;
}


function canAssignTransaction(transition, transactionId){

	var returnValue = false;
	var firstTransitionOfTransaction = true;
	
	var transitions = application.getElementsByClassName(Transition);
	
	$(transitions).each(function(i,t){
	
		if (t.transaction!=null){	
			if (t.transaction.getId() == transactionId){
				firstTransitionOfTransaction = false;
			
				if (t.target == transition.source || t.source == transition.target){
						returnValue = true; 
						return false;//break;
				}
				
			}
		}
			
	});

	if (firstTransitionOfTransaction==true)
		return true;

	return returnValue;
}

function htmlspecialchars (string, quote_style, charset, double_encode) {
    // Convert special characters to HTML entities  
    // 
    // version: 912.1315
    // discuss at: http://phpjs.org/functions/htmlspecialchars    // +   original by: Mirek Slugen
    // +   improved by: Kevin van Zonneveld (http://kevin.vanzonneveld.net)
    // +   bugfixed by: Nathan
    // +   bugfixed by: Arno
    // +    revised by: Kevin van Zonneveld (http://kevin.vanzonneveld.net)    // +    bugfixed by: Brett Zamir (http://brett-zamir.me)
    // +      input by: Ratheous
    // +      input by: Mailfaker (http://www.weedem.fr/)
    // +      reimplemented by: Brett Zamir (http://brett-zamir.me)
    // +      input by: felix    // +    bugfixed by: Brett Zamir (http://brett-zamir.me)
    // %        note 1: charset argument not supported
    // *     example 1: htmlspecialchars("<a href='test'>Test</a>", 'ENT_QUOTES');
    // *     returns 1: '&lt;a href=&#039;test&#039;&gt;Test&lt;/a&gt;'
    // *     example 2: htmlspecialchars("ab\"c'd", ['ENT_NOQUOTES', 'ENT_QUOTES']);    // *     returns 2: 'ab"c&#039;d'
    // *     example 3: htmlspecialchars("my "&entity;" is still here", null, null, false);
    // *     returns 3: 'my &quot;&entity;&quot; is still here'
    var optTemp = 0, i = 0, noquotes= false;
    if (typeof quote_style === 'undefined' || quote_style === null) {        quote_style = 2;
    }
    string = string.toString();
    if (double_encode !== false) { // Put this first to avoid double-encoding
        string = string.replace(/&/g, '&amp;');    }
    string = string.replace(/</g, '&lt;').replace(/>/g, '&gt;');
 
    var OPTS = {
        'ENT_NOQUOTES': 0,        'ENT_HTML_QUOTE_SINGLE' : 1,
        'ENT_HTML_QUOTE_DOUBLE' : 2,
        'ENT_COMPAT': 2,
        'ENT_QUOTES': 3,
        'ENT_IGNORE' : 4    };
    if (quote_style === 0) {
        noquotes = true;
    }
    if (typeof quote_style !== 'number') { // Allow for a single string or an array of string flags        quote_style = [].concat(quote_style);
        for (i=0; i < quote_style.length; i++) {
            // Resolve string input to bitwise e.g. 'PATHINFO_EXTENSION' becomes 4
            if (OPTS[quote_style[i]] === 0) {
                noquotes = true;            }
            else if (OPTS[quote_style[i]]) {
                optTemp = optTemp | OPTS[quote_style[i]];
            }
        }        quote_style = optTemp;
    }
    if (quote_style & OPTS.ENT_HTML_QUOTE_SINGLE) {
        string = string.replace(/'/g, '&#039;');
    }    if (!noquotes) {
        string = string.replace(/"/g, '&quot;');
    }
 
    return string;
	
}
	
	
	
/*------------------------------------------------------------------------------------------------------------------
	Add scrollstart and scrolllstop events to jquery libs
------------------------------------------------------------------------------------------------------------------*/
/*
(function(){
 
    var special = jQuery.event.special,
        uid1 = 'D' + (+new Date()),
        uid2 = 'D' + (+new Date() + 1);
 
    special.scrollstart = {
        setup: function() {
 
            var timer,
                handler =  function(evt) {
 
                    var _self = this,
                        _args = arguments;
 
                    if (timer) {
                        clearTimeout(timer);
                    } else {
                        evt.type = 'scrollstart';
                        jQuery.event.handle.apply(_self, _args);
                    }
 
                    timer = setTimeout( function(){
                        timer = null;
                    }, special.scrollstop.latency);
 
                };
 
            jQuery(this).bind('scroll', handler).data(uid1, handler);
 
        },
        teardown: function(){
            jQuery(this).unbind( 'scroll', jQuery(this).data(uid1) );
        }
    };
 
    special.scrollstop = {
        latency: 300,
        setup: function() {
 
            var timer,
                    handler = function(evt) {
 
                    var _self = this,
                        _args = arguments;
 
                    if (timer) {
                        clearTimeout(timer);
                    }
 
                    timer = setTimeout( function(){
 
                        timer = null;
                        evt.type = 'scrollstop';
                        jQuery.event.handle.apply(_self, _args);
 
                    }, special.scrollstop.latency);
 
                };
 
            jQuery(this).bind('scroll', handler).data(uid2, handler);
 
        },
        teardown: function() {
            jQuery(this).unbind( 'scroll', jQuery(this).data(uid2) );
        }
    };
 
})();
*/




