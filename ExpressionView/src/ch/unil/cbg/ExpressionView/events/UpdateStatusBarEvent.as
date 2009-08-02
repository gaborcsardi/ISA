package ch.unil.cbg.ExpressionView.events {
	
	import flash.events.Event;
	
	public class UpdateStatusBarEvent extends Event {

		// statusbar content
		public var statusbarcontent:String
		public static const UPDATESTATUSBAREVENT:String = "UpdateStatusBarEvent";
		
		public function UpdateStatusBarEvent(_statusbarcontent: String) {
			super(UPDATESTATUSBAREVENT, true, true);
			statusbarcontent = _statusbarcontent;
		}
		
		override public function clone(): Event {
			return new UpdateStatusBarEvent(statusbarcontent);
		}

	}
}