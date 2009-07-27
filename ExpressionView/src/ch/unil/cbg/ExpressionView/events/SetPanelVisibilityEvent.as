package ch.unil.cbg.ExpressionView.events {
	
	import flash.events.Event;
	
	public class SetPanelVisibilityEvent extends Event {

		// file data
		public var panel:String;		
		public var visible:Boolean;
		public static const SETPANELVISIBILITYEVENT:String = "SetPanelVisibilityEvent";
		
		public function SetPanelVisibilityEvent(_panel:String, _visible:Boolean) {
			super(SETPANELVISIBILITYEVENT, true, true);
			panel = _panel;
			visible = _visible;
		}
		
		override public function clone(): Event {
			return new SetPanelVisibilityEvent(panel, visible);
		}

	}
}