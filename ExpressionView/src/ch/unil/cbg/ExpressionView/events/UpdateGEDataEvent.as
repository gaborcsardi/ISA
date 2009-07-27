package ch.unil.cbg.ExpressionView.events {
	
	//import ch.unil.cbg.ExpressionView.model.GeneExpressionData;
	
	import flash.events.Event;
	
	public class UpdateGEDataEvent extends Event {

		// file data
		public var data:Array
		public static const UPDATEGEDATAEVENT:String = "UpdateGEDataEvent";
		
		public function UpdateGEDataEvent(_data: Array) {
			super(UPDATEGEDATAEVENT, true, true);
			data = _data;
		}
		
		override public function clone(): Event {
			return new UpdateGEDataEvent(data);
		}

	}
}