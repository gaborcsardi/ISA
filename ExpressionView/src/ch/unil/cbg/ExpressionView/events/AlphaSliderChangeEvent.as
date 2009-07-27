package ch.unil.cbg.ExpressionView.events {
	
	import flash.events.Event;
	
	public class AlphaSliderChangeEvent extends Event {

		// file data
		public var alpha:Number;
		
		public static const ALPHASLIDERCHANGEEVENT:String = "AlphaSliderChangeEvent";
		
		public function AlphaSliderChangeEvent(_alpha: Number) {
			super(ALPHASLIDERCHANGEEVENT, true, true);
			alpha = _alpha;
		}
		
		override public function clone(): Event {
			return new AlphaSliderChangeEvent(alpha);
		}

	}
}