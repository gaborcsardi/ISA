package ch.unil.cbg.ExpressionView.events {
	
	import flash.events.Event;
	
	public class BroadcastPositionEvent extends Event {

		public static const MOUSE_OVER:String = "MouseOver";
		public static const MOUSE_CLICK:String = "MouseClick";
		public static const MOUSE_DOUBLE_CLICK:String = "MouseDubleClick";

		// data
		public var data:Array;
		
		public function BroadcastPositionEvent(_type:String, _data:Array=null, _bubbles:Boolean=true, _cancelable:Boolean=true) {
			super(_type, _bubbles, _cancelable);
			data = _data;
		}
		
		override public function clone(): Event {
			return new BroadcastPositionEvent(type, data, bubbles, cancelable);
		}

	}
}