/**
	@class TransitionLabelLocator
*/
TransitionLabelLocator=function(/*:draw2d.Connection*/ connection){
  draw2d.ConnectionLocator.call(this,connection);
}

TransitionLabelLocator.prototype = new draw2d.ConnectionLocator;
TransitionLabelLocator.prototype.type="TransitionLabelLocator";


/**
  Relocates the given Label setting it near a given point of draw2d.Connection
  @param {draw2d.Figure} target - The figure to relocate
*/
TransitionLabelLocator.prototype.relocate=function(/*:draw2d.Figure*/ target){

	var conn = this.getConnection();
	var p = new draw2d.Point();
	var points = conn.getPoints();
	var index;
	var p1;
	var p2;
	var automatic = false;

	if (!automatic){
		(conn.getRouter().transitionToSameState == true) ? index=3 : index=1;
		p1 = points.get(index);
		p2 = points.get(index);

		p.x = (p2.x - p1.x) / 2 + p1.x -10 ;
		p.y = (p2.y - p1.y) / 2 + p1.y -4 ;
	}else{
		index = Math.floor((points.getSize() -2) / 2);
		p1 = points.get(index);
		p2 = points.get(index + 1);

		p.x = (p2.x - p1.x) / 2 + p1.x -10 ;
		p.y = (p2.y - p1.y) / 2 + p1.y -4 ;
	}


	target.setPosition(p.x,p.y);

}
