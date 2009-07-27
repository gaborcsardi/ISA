package ch.unil.cbg.ExpressionView.events {
	
	import flash.events.Event;
	
	public class ToggleFullScreenEvent extends Event {

		// file data
		public var screen:String
		public static const TOGGLEFULLSCREENEVENT:String = "ToggleFullScreenEvent";
		
		public function ToggleFullScreenEvent(_screen: String) {
			super(TOGGLEFULLSCREENEVENT, true, true);
			screen = _screen;
		}
		
		override public function clone(): Event {
			return new ToggleFullScreenEvent(screen);
		}

	}
}