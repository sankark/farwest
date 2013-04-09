/**
  @class Routes a draw2d.Connection beetween two States or to the same State.
*/
TransitionRouter=function( source, target, connection ){
	this.transitionToSameState = (source.parentNode ==  target.parentNode) ? true : false;
	this.separation = 0;

	var sourceConnections = new draw2d.ArrayList();
	var targetConnections = new draw2d.ArrayList();
	var sharedConnections = new draw2d.ArrayList();

	if (this.transitionToSameState == false){
		connection.setTargetDecorator(new draw2d.ArrowConnectionDecorator());
		connection.setSourceAnchor(new draw2d.ChopboxConnectionAnchor());
		connection.setTargetAnchor(new draw2d.ChopboxConnectionAnchor());
		connection.targetDecorator.setBackgroundColor( source.parentNode.getBackgroundColor() );
	}


	//If not, expand the draw2d.ArrayList class
	if (!(draw2d.ArrayList.prototype.addSecure))
		draw2d.ArrayList.prototype.addSecure = function(obj){
		   if ( this.contains(obj) == false){
			   if(this.getSize() == this.data.length) {
				  this.resize();
			   }
			   this.data[this.size++] = obj;
		   }
		}


	for (i=0; i<source.parentNode.getPorts().getSize(); i++)
		for (x=0; x< source.parentNode.getPorts().get(i).getConnections().getSize(); x++)
			if (source.parentNode.getPorts().get(i).getConnections().get(x).getRouter().transitionToSameState == this.transitionToSameState)
				sourceConnections.add( source.parentNode.getPorts().get(i).getConnections().get(x) );


	for (i=0; i<target.parentNode.getPorts().getSize(); i++)
		for (x=0; x< target.parentNode.getPorts().get(i).getConnections().getSize(); x++)
			if (target.parentNode.getPorts().get(i).getConnections().get(x).getRouter().transitionToSameState == this.transitionToSameState)
				targetConnections.add( target.parentNode.getPorts().get(i).getConnections().get(x) );


	for (i=0; i<sourceConnections.getSize(); i++)
		if ( targetConnections.contains(sourceConnections.get(i)) )
			sharedConnections.addSecure(sourceConnections.get(i));

	for (i=0; i<targetConnections.getSize(); i++)
		if ( sourceConnections.contains(targetConnections.get(i)) )
			sharedConnections.addSecure(targetConnections.get(i));


	this.separation = sharedConnections.getSize();

	sharedConnections = null;
	sourceConnections = null;
	targetConnections = null;

}

TransitionRouter.prototype = new draw2d.NullConnectionRouter;
TransitionRouter.prototype.type="TransitionRouter";


/** @private */
TransitionRouter.prototype.route=function(/*:draw2d.Connection*/ conn){

	(this.transitionToSameState==true) ? this.routeToSelfState(conn,this.separation) : this.routeCollision(conn,this.separation);

}


/** @private */
TransitionRouter.prototype.routeNormal=function(/*:draw2d.Connection*/ conn){
	conn.addPoint(conn.getStartPoint());
	conn.addPoint(conn.getEndPoint());
}


/** @private */
TransitionRouter.prototype.routeCollision=function(/*:draw2d.Connection*/ conn, /*:int*/ index){
   var start = conn.getStartPoint();
   var end = conn.getEndPoint();


   conn.addPoint(start);

   var separation = 30;


   var midPoint = new draw2d.Point((end.x + start.x) / 2, (end.y + start.y) / 2);
   var position = end.getPosition(start);
   var ray;
   if (position ==draw2d.PositionConstants.SOUTH || position == draw2d.PositionConstants.EAST)
      ray = new draw2d.Point( end.x - start.x, end.y - start.y);
   else
      ray = new draw2d.Point( start.x - end.x, start.y - end.y);

   var length = Math.sqrt(ray.x*ray.x+ray.y*ray.y);

   var xSeparation = separation * ray.x / length;
   var ySeparation = separation * ray.y / length;

   var bendPoint;

   if (index % 2 == 0)
      bendPoint = new draw2d.Point( midPoint.x + (index / 2) * (-1 * ySeparation), midPoint.y + (index / 2) * xSeparation);
   else
      bendPoint = new draw2d.Point(midPoint.x + (index / 2) * ySeparation, midPoint.y + (index / 2) * (-1 * xSeparation));

   conn.addPoint(bendPoint);
   conn.addPoint(end);
}


/** @private */
TransitionRouter.prototype.routeToSelfState=function(/*:draw2d.Connection*/ conn, index){
	var real_start = conn.getStartPoint();
	var real_end = conn.getEndPoint();

	var stateW = conn.getTarget().parentNode.width;
	var stateH = conn.getTarget().parentNode.height;

    var position = real_end.getPosition(real_start);
    if (position == draw2d.PositionConstants.EAST){
		var temp = real_start;
		real_start = real_end;
		real_end = temp;
	}

	var start = new draw2d.Point( real_start.x + (stateW/2), real_start.y - (stateH/2) );
	var end = new draw2d.Point( real_end.x - (stateW/2), real_end.y + (stateH/2) );

	var separation = 12 * index;

	var xDiff = 60 + separation;
	var yDiff = 10 + separation;


	conn.addPoint(start);

	if (position == draw2d.PositionConstants.EAST){
		xDiff = -xDiff;
	}

	var p1 = new draw2d.Point( start.x , start.y - yDiff );
	var p2 = new draw2d.Point( start.x - xDiff , start.y - yDiff );
	var p3 = new draw2d.Point( end.x - xDiff , end.y + yDiff );
	var p4 = new draw2d.Point( end.x , end.y + yDiff );

	conn.addPoint(p1);
	conn.addPoint(p2);
	conn.addPoint(p3);
	conn.addPoint(p4);

	conn.addPoint(end);

}