package ch.unil.cbg.ExpressionView.events {
	
	import flash.events.Event;
	
	public class HighlightingEvent extends Event {

		// what to highlight
		public static const MODULE:String = "Module";
		public static const GENE:String = "Gene";
		public static const SAMPLE:String = "Sample";

		// data
		public var data:Array;		
		
		public function HighlightingEvent(_type:String, _data:Array=null, _bubbles:Boolean=true, _cancelable:Boolean=true) {
			super(_type, _bubbles, _cancelable);
			data = _data;
		}
		
		override public function clone(): Event {
			return new HighlightingEvent(type, data, bubbles, cancelable);
		}

	}
}