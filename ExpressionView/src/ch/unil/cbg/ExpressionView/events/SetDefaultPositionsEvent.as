package ch.unil.cbg.ExpressionView.events {
	
	import flash.events.Event;
	
	public class SetDefaultPositionsEvent extends Event {

		public static const SETDEFAULTPOSITIONSEVENT:String = "SetDefaultPositionsEvent";
		
		public function SetDefaultPositionsEvent() {
			super(SETDEFAULTPOSITIONSEVENT, true, true);
		}
		
		override public function clone(): Event {
			return new SetDefaultPositionsEvent();
		}

	}
}