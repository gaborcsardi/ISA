package ch.unil.cbg.ExpressionView.events {
	
	import flash.events.Event;
	
	public class BroadcastInspectPositionEvent extends Event {

		// position on screen
		public var gene:int;
		public var sample:int;
		public static const BROADCASTINSPECTPOSITIONEVENT:String = "BroadcastInspectPositionEvent";
		
		public function BroadcastInspectPositionEvent(_gene:int, _sample: int) {
			super(BROADCASTINSPECTPOSITIONEVENT, true, true);
			gene = _gene;
			sample = _sample;
		}
		
		override public function clone(): Event {
			return new BroadcastInspectPositionEvent(gene, sample);
		}

	}
}