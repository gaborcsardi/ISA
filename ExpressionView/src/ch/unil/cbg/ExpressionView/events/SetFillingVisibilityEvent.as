package ch.unil.cbg.ExpressionView.events {
	
	import flash.events.Event;
	
	public class SetFillingVisibilityEvent extends Event {

		// file data
		public var visible:Boolean;
		
		public static const SETFILLINGVISIBILITYEVENT:String = "SetFillingVisibilityEvent";
		
		public function SetFillingVisibilityEvent(_visible: Boolean) {
			super(SETFILLINGVISIBILITYEVENT, true, true);
			visible = _visible;
		}
		
		override public function clone(): Event {
			return new SetFillingVisibilityEvent(visible);
		}

	}
}