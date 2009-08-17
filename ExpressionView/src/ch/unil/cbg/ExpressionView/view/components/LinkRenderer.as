package ch.unil.cbg.ExpressionView.view.components {
	
	import flash.events.MouseEvent;
	import flash.net.URLRequest;
	import flash.net.navigateToURL;
	
	import mx.controls.Label;
	import mx.controls.listClasses.IDropInListItemRenderer;
	import mx.core.IDataRenderer;

	public class LinkRenderer extends Label implements IDataRenderer, IDropInListItemRenderer {

		public var url:String;
				
		public function LinkRenderer() {
			super();
			url = new String();
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
			//trace(listData.columnIndex);
			//trace(listData.label);
			//var request:URLRequest = new URLRequest(url + listData.label);
			//trace(event.currentTarget.data);
			var request:URLRequest = new URLRequest(url + event.currentTarget.data.tag3);
			navigateToURL(request, "_blank");
		}
				
	}
}