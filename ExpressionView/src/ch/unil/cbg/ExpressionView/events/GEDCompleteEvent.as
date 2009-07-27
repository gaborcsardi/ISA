package ch.unil.cbg.ExpressionView.events {
	
	import flash.events.Event;
	
	public class GEDCompleteEvent extends Event {

		public static const GEDCOMPLETEEVENT:String = "GEDCompleteEvent";
		
		public function GEDCompleteEvent() {
			super(GEDCOMPLETEEVENT, true, true);
		}
		
		override public function clone(): Event {
			return new GEDCompleteEvent();
		}

	}
}