package ch.unil.cbg.ExpressionView.events {
	
	import flash.events.Event;
	
	public class ClosableTabNavigatorEvent extends Event {

		// events
		public static const CLOSE:String = "Close";

		// data
		public var data:Array;
		
		public function ClosableTabNavigatorEvent(_type:String, _data:Array=null, _bubbles:Boolean=true, _cancelable:Boolean=true) {
			super(_type, _bubbles, _cancelable);
			data = _data;
		}
		
		override public function clone(): Event {
			return new ClosableTabNavigatorEvent(type, data, bubbles, cancelable);
		}

	}
}