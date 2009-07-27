package ch.unil.cbg.ExpressionView.events {
	
	import flash.events.Event;
	
	public class PDFExportEvent extends Event {

		public static const PDFEXPORTEVENT:String = "PDFExportEvent";
		
		public function PDFExportEvent() {
			super(PDFEXPORTEVENT, true, true);
		}
		
		override public function clone(): Event {
			return new PDFExportEvent();
		}

	}
}