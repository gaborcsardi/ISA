package ch.unil.cbg.ExpressionView.events {
	
	import flash.events.Event;
	
	public class ModeChangeEvent extends Event {

		// file data
		public var mode:int;
		
		public static const MODECHANGEEVENT:String = "ModeChangeEvent";
		
		public function ModeChangeEvent(_mode: int) {
			super(MODECHANGEEVENT, true, true);
			mode = _mode;
		}
		
		override public function clone(): Event {
			return new ModeChangeEvent(mode);
		}

	}
}