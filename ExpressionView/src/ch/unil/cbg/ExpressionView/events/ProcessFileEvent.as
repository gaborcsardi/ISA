package ch.unil.cbg.ExpressionView.events {
	
	import flash.events.Event;
	import flash.utils.ByteArray;
	
	public class ProcessFileEvent extends Event {

		// file data
		public var filecontent:ByteArray;
		public static const PROCESSFILEEVENT:String = "ProcessFileEvent";
		
		public function ProcessFileEvent(_filecontent: ByteArray) {
			super(PROCESSFILEEVENT, true, true);
			filecontent = _filecontent;
		}
		
		override public function clone(): Event {
			return new ProcessFileEvent(filecontent);
		}

	}
}