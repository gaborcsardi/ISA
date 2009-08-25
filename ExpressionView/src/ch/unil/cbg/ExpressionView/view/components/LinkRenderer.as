package ch.unil.cbg.ExpressionView.view.components {
	
	import flash.events.MouseEvent;
	import flash.net.URLRequest;
	import flash.net.navigateToURL;
	
	import mx.controls.Label;
	import mx.controls.listClasses.IDropInListItemRenderer;
	import mx.core.IDataRenderer;

	public class LinkRenderer extends Label implements IDataRenderer, IDropInListItemRenderer {

		public var dataProvider:String;
				
		public function LinkRenderer() {
			super();
			dataProvider = new String();
		}

		override protected function createChildren():void {
			super.createChildren();
			addEventListener(MouseEvent.MOUSE_OVER, mouseOverHandler);
			addEventListener(MouseEvent.MOUSE_OUT, mouseOutHandler);
			addEventListener(MouseEvent.CLICK, mouseClickHandler);
		}
		
		private function mouseOverHandler(event:MouseEvent):void {
		   event.currentTarget.setStyle("color","0x0000FF");
		   event.currentTarget.setStyle("textDecoration","underline");
		}
 
		private function mouseOutHandler(event:MouseEvent):void {
		   event.currentTarget.setStyle("color","0x000000");
		   event.currentTarget.setStyle("textDecoration","none");
		}
 
		private function mouseClickHandler(event:MouseEvent):void {
			var url:String = new String();
			if ( dataProvider == "genecard" ) {
				url = "http://www.genecards.org/cgi-bin/carddisp.pl?gene=" + event.currentTarget.data.tag2;
			} else if ( dataProvider == "entrez" ) {
				url = "http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene&cmd=Retrieve&dopt=full_report&list_uids=" + event.currentTarget.data.tag3; 
			} else if ( dataProvider == "go" ) {
				url = "http://amigo.geneontology.org/cgi-bin/amigo/term-details.cgi?term=" + event.currentTarget.data.tag1
			}
			var request:URLRequest = new URLRequest(url);
			navigateToURL(request, "_blank");
		}
				
	}
}