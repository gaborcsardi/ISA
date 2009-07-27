package ch.unil.cbg.ExpressionView.events {
	
	import flash.events.Event;
	
	public class SetOutlineVisibilityEvent extends Event {

		// file data
		public var visible:Boolean;
		
		public static const SETOUTLINEVISIBILITYEVENT:String = "SetOutlineVisibilityEvent";
		
		public function SetOutlineVisibilityEvent(_visible: Boolean) {
			super(SETOUTLINEVISIBILITYEVENT, true, true);
			visible = _visible;
		}
		
		override public function clone(): Event {
			return new SetOutlineVisibilityEvent(visible);
		}

	}
}