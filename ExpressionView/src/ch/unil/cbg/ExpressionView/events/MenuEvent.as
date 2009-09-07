package ch.unil.cbg.ExpressionView.events {
	
	import flash.events.Event;
	
	public class MenuEvent extends Event {

		// menu item
		public static const OPEN:String = "Open";
		
		public static const MODE:String = "Behavior";
		
		public static const OUTLINE:String = "Outline";
		public static const FILLING:String = "Filling";
		public static const ALPHA:String = "Alpha";
		
		public static const PDF_EXPORT:String = "PDFExport";
		public static const EXCEL_EXPORT:String = "ExcelExport";
		
		public static const PANELS:String = "Panels";
		
		public static const DEFAULT_POSITIONS:String = "DefaultPositions";
		public static const FULLSCREEN:String = "Fullscreen";

		// data
		public var data:Array;		
		
		public function MenuEvent(_type:String, _data:Array=null, _bubbles:Boolean=true, _cancelable:Boolean=true) {
			super(_type, _bubbles, _cancelable);
			data = _data;
		}
		
		override public function clone(): Event {
			return new MenuEvent(type, data, bubbles, cancelable);
		}

	}
}