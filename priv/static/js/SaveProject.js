XMLExport = function(){}


/** @static */
XMLExport.isValid = function(){
	
	var null_state = false;
	var initials = 0;
	
	
	$(application.getElementsByClassName(State)).each(function(i,state){
		if (state.page == null){
			MessageDialog.show( 'Error', 'State <i>"'+state.name+'"</i> must be in a page.', 'error-icon');
			null_state=true;
			return false; //break;
		}

		
		if (state.initial == true)
			initials++;
	});
	
	if (null_state)
		return false;
		
	if (initials==0){
		MessageDialog.show( 'Error', 'Missing initial state.', 'error-icon');
		return false;
	}
	
	if (initials>1){
		MessageDialog.show( 'Error', 'More than one state is marked as initial.', 'error-icon');
		return false;
	}
	
	
	
	var wrong_state_pos = false;
	var hasTriggerEvents = false;
	var gotTransitions = false;
	
	$(application.getElementsByClassName(Transition)).each(function(i,tr){
		gotTransitions = true;
		hasTriggerEvents = false;
		
		$(tr.getTriggerEvents()).each(function(k,te){
			hasTriggerEvents = true;
			if (te.graph_elem.page != tr.source.page){
				MessageDialog.show( 'Error', 'State <i>'+tr.source.name+'</i> should be in page <i>'+te.graph_elem.page.name+'</i> because transition <i>'+tr.name+'</i> is triggered by an event based on a <i>'+te.graph_elem.page.name+'</i>\'s graphical element.', 'error-icon');
				wrong_state_pos = true;
				return false; //break;
			}
		});
		
		$(tr.getTriggeredActions()).each(function(k,ac){
			var sourceTransitionPage = tr.source.page;
			$(ac.getParameters()).each(function(w,acParam){
				if (acParam.source instanceof GraphicalElement && acParam.source.page!=sourceTransitionPage){
					MessageDialog.show( 'Error', 'State <i>'+tr.source.name+'</i> should be in page <i>'+acParam.source.page+'</i> because transition <i>'+tr.name+'</i> trigger an action based on a <i>'+acParam.source.page+'</i>\'s graphical element.', 'error-icon');
					wrong_state_pos = true;
					return false; //break;	
				}
			});
		});	
		
		if (hasTriggerEvents==false){
			MessageDialog.show( 'Error', 'Nothing trigger transition <i>'+tr.name+'</i>.', 'error-icon');		
			return false; //break;
		}
		
		if (wrong_state_pos==true)
			return false; //break;
	});
	
	if (gotTransitions==false){
		MessageDialog.show( 'Warning', 'There are no transitions!', 'error-icon');
		return false;
	}
	
	if (wrong_state_pos==true)
		return false;
			
	if (hasTriggerEvents==false)
		return false;
		
	return true;
}


/** @static */
XMLExport.execute = function(save_as){
	


	var generate_xml_document = function(appname){
	
		var xml = '';
		var serialize_only_models = false;
		var append = function( str ){ xml += str + '\n'; }
		var getFigureXMLParams = function(fig){ return (serialize_only_models==true) ? '' : ' FigX="'+fig.getX()+'" FigY="'+fig.getY()+'" FigHeight="'+fig.getHeight()+'" FigWidth="'+fig.getWidth()+'" ' }//possono essere utili anche fig.getId e fig.getZorder
		
		var parts = appname.toLowerCase().split('.');
		if (parts.length>0 && parts[parts.length-1]=='xml'){
			parts.splice(parts.length-1);
			appname = parts.join();
		}
		
		append('<Application name="'+appname+'">');
		
		
		$(application.getElementsByClassName(Page)).each(function(i,page){
			
			append('<Page alias="'+page.name+'" url="'+page.url+'" id="'+page.getId()+'" '+(getFigureXMLParams(page.getParent()))+' >');
			
				$(page.getGraphicalElements()).each( function(k,graph_elem){
					append('	<GraphicalElement name="'+graph_elem.graphelem_name+'" type="'+graph_elem.graphelem_type+'" id="'+graph_elem.getId()+'"/>');
				});
				
				$(page.getStates()).each( function(k,state){
					append('	<State name="'+state.name+'" initial="'+state.initial+'" id="'+state.getId()+'" '+(getFigureXMLParams(state.getParent()))+' >');
						
						$(state.getParameters() ).each( function(j,param){
							append('		<StateParameter name="'+param.param_name+'" type="'+param.param_type+'" id="'+param.getId()+'" />');
						});
					
					append('	</State>');
				});
				
			append('</Page>');
			append('\n');
			
		});
		
		
		var appendTransition = function(transition){
			append('	<Transition name="'+transition.name+'" sourceStateId="'+transition.source.getId()+'" targetStateId="'+transition.target.getId()+'" id="'+transition.getId()+'" >');
				
				$(transition.getTriggerEvents()).each( function(k,trig_event){
					append('		<TriggerEvent graphicalElementId="'+trig_event.graph_elem.getId()+'" eventType="'+trig_event.event_type+'" id="'+trig_event.getId()+'" />');
				});
				
				$(transition.getTriggeredActions()).each( function(k,action){
					append('		<Action id="'+action.getId()+'" type="'+action.getType()+'">');
						
					$(action.getParameters()).each( function(f,actionParameter){
							append('			<ActionParameter name="'+actionParameter.name+'" type="'+actionParameter.type+'" sourceId="'+actionParameter.source.getId()+'" />');
					});
					append('		</Action>');
					
				});	
				
			append('	</Transition>');
		}
		
		
		var transitions = application.getElementsByClassName(Transition);
		var transactions =  application.getElementsByClassName(Transaction);
		var _Transaction = function(transaction,transitions){ this.transaction = transaction; this.transitions = transitions }
		var alone_transitions = new Array();
		
		
		for(i=0; i<transactions.length; i++){
			transactions[i] = new _Transaction(transactions[i],new Array());
		}
		
		
		$(transitions).each(function(i,transition){
			if (!(transition.transaction instanceof Transaction)){ 
				alone_transitions.push(transition);
			}else{
				$(transactions).each(function(i,_transaction){
					if ( _transaction.transaction == transition.transaction ){
						_transaction.transitions.push( transition );
					}
				});		
			}
		});
		
		
		//append transitions binded to a transaction
		$(transactions).each(function(i,_transaction){	
			append('<Transaction name="'+_transaction.transaction.name+'" id="'+_transaction.transaction.getId()+'">');
			$(_transaction.transitions).each(function(i,transition){
				appendTransition(transition);
			});
			append('</Transaction>');
			append('\n');
		});		
			
			
		//append alone transitions
		if (alone_transitions.length!=0){
			append('<Transitions>');
			$(alone_transitions).each(function(i,transition){
				appendTransition(transition);
			});
			append('</Transitions>');
			append('\n');
		}
		
		//append DbConnectionDescriptors (use default if not one)
		var dbConnectionDescriptors = application.getElementsByClassName(DbConnectionDescriptor);
		var mainDbConnectionDescriptor = (dbConnectionDescriptors.length==0) ? application.createMainDbConnectionDescriptor() : dbConnectionDescriptors[0];	
		append('<DbConnectionDescriptor driver="'+mainDbConnectionDescriptor.driver+'" url="'+mainDbConnectionDescriptor.url+'" user="'+mainDbConnectionDescriptor.user+'" pass="'+mainDbConnectionDescriptor.pass+'" id="'+mainDbConnectionDescriptor.getId()+'" />');
		append('\n');
		
		
		append('</Application>');
		
		return xml;
	};
	
	
	var gen_project = function(projectname){
		
		$.ajax({
			url: "generate.jsp",
			type: "POST",
			processData: true,
			data: { project_filename: projectname },
			dataType: "text",
			beforeSend: function (request) {
				MessageDialog.show( 'Generate site structure', '<table style="width:auto;"><tr><td><img src="src/theme/gears.gif" alt=""/></td><td>Please wait...</td></tr></table>');
			},
			success: function(data, textStatus){
				data = $.trim(data);
				var isOK = (data.length!=0 && data.substr(0,2).toLowerCase()=='ok') ? true : false;
				var appURL = ( isOK==true ) ? data.substr(3) : null;
				
				if (isOK){
					MessageDialog.show( 'Generate site structure', '<strong>Done!</strong><br/><a href="'+appURL+'" target="_blank">Check it out</a><br/><br/><div style="color:gray">Note: Your session has been invalidated, then the other opened web applications will be resetted.<br/>If the resulting application contains actions that act on a database, it is necessary that the database exists and is consistent with the parameters of actions.</div>','success-icon');
				}else{
					MessageDialog.show( 'Generate site structure', '<strong>Unable to generate site structure.</strong><br/><i>Server say:</i> '+data, 'error-icon');
				}
			},
			error: function (request, textStatus, errorThrown) {
				MessageDialog.show( 'Save project', '<strong>Unable to generate site structure.</strong><br/>Ajax error: '+textStatus, 'error-icon');
			}
		});
		
	};
	
	var save_project = function(projectname,canoverwrite){
		
		//generate xml
		var xml = generate_xml_document(projectname);	

		var no_errors = true;
		
		$.ajax({
			url: "save.jsp",
			type: "POST",
			processData: true,
			data: { project_name: projectname, content_len: xml.length, content_xml: xml, can_overwrite: canoverwrite },
			dataType: "text",
			beforeSend: function (request) {
				MessageDialog.show( 'Save project', 'Please wait...' );
			},
			success: function(data, textStatus){
				data = $.trim(data);
				var is_saved = (data.length!=0 && data.substr(0,2).toLowerCase()=='ok') ? true : false;
				var saved_filename = ( is_saved==true ) ? data.substr(3) : null;
				if (is_saved){
					var msgHtml = '<strong>Project saved.</strong>';
					msgHtml += '<table style="width:auto: height:auto; margin:0px; padding:0px; padding-left:20px;">';
					
					msgHtml += '<tr><td><div class="ui-state-default ui-corner-all" title=""><span class="ui-icon ui-icon-document"></span></div></td>';
					msgHtml += '<td><a href="view.jsp?project_filename='+saved_filename+'" target="_blank"> View generated XML code</a></td></tr>';
					
					msgHtml += '<tr><td><div class="ui-state-default ui-corner-all" title=""><span class="ui-icon ui-icon-gear"></span></div></td>';
					msgHtml += '<td><a href="#" id="_gen_site_bt_">Generate site structure</a></td></tr>';
					
					msgHtml += '</table>';
					MessageDialog.show( 'Save project', msgHtml, 'success-icon');
					
					$('#_gen_site_bt_').click( function(e){
						e.preventDefault();
						gen_project(saved_filename);
					});
					
				}else{
					MessageDialog.show( 'Save project', '<strong>Unable to save.</strong><br/><i>Server say:</i> '+data, 'error-icon');
					no_errors = false;
				}
			},
			error: function (request, textStatus, errorThrown) {
				MessageDialog.show( 'Save project', '<strong>Unable to save.</strong><br/>Ajax error: '+textStatus, 'error-icon');
				no_errors = false;
			}
		});
		
		return no_errors;
	}
	
	
	//check if the document is valid
	if (XMLExport.isValid() == false)
		return;
	
	
	if (XMLImport.currentProject==null || ((save_as) && (save_as==true)) ){
		//ask for project name
		new InputDialog(
			'Save project',
			'Project name:',
			function(input_txt){ 
				if (save_project(input_txt,"NO")==true){
					XMLImport.currentProject = input_txt;
				}
			}
		);
	}else{
		save_project(XMLImport.currentProject,"YES");
	}

}

