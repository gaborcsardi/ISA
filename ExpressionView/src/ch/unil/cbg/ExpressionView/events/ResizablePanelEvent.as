package ch.unil.cbg.ExpressionView.events {
	
	import flash.events.Event;
	
	public class ResizablePanelEvent extends Event {

		// events
		public static const CLOSE:String = "Close";
		public static const MINIMIZE:String = "Minimize";
		public static const MAXIMIZE:String = "Maximize";
		public static const RESIZE:String = "REsize";

		// data
		public var data:Array;
		
		public function ResizablePanelEvent(_type:String, _data:Array=null, _bubbles:Boolean=true, _cancelable:Boolean=true) {
			super(_type, _bubbles, _cancelable);
			data = _data;
		}
		
		override public function clone(): Event {
			return new ResizablePanelEvent(type, data, bubbles, cancelable);
		}

	}
}