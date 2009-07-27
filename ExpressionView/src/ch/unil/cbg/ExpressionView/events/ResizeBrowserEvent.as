package ch.unil.cbg.ExpressionView.events {
	
	import flash.events.Event;
	
	public class ResizeBrowserEvent extends Event {

		public var scaleX:Number;
		public var scaleY:Number;
		public static const RESIZEBROWSEREVENT:String = "ResizeBrowserEvent";
		
		public function ResizeBrowserEvent(_scaleX:Number, _scaleY:Number) {
			super(RESIZEBROWSEREVENT, true, true);
			scaleX = _scaleX;
			scaleY = _scaleY;
		}
		
		override public function clone(): Event {
			return new ResizeBrowserEvent(scaleX, scaleY);
		}

	}
}