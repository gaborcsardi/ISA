package ch.unil.cbg.ExpressionView.view {

	import ch.unil.cbg.ExpressionView.events.*;
	
	import mx.containers.Canvas;
	import mx.containers.HBox;
	import mx.controls.Text;
			
	public class StatusBar extends Canvas {

		public var status:String = ""; 
		
		private var footerBox:HBox;
		private var footerText:Text;
		
		public function StatusBar() {
			super();
		}
		
		private function updateStatusBarEventHandler(event:UpdateStatusBarEvent):void {
			footerText.text = event.statusbarcontent;
			invalidateProperties();
		}

		// layout		
		override protected function createChildren():void {
			super.createChildren();
			parentApplication.addEventListener(UpdateStatusBarEvent.UPDATESTATUSBAREVENT, updateStatusBarEventHandler);
			
			if ( !footerBox ) {
				footerBox = new HBox();
				footerBox.setStyle("verticalAlign", "middle");				
				addChild(footerBox);
				
				if ( !footerText ) {
					footerText = new Text();
					footerBox.addChild(footerText);
				}
			}
						
		}
		
		override protected function updateDisplayList(unscaledWidth:Number, unscaledHeight:Number):void {
			super.updateDisplayList(unscaledWidth, unscaledHeight);
		
			footerBox.percentWidth = 100;
			footerBox.percentHeight = 100;
			footerText.percentWidth = 100;
			footerText.percentHeight = 100;
		}
		
		override protected function commitProperties(): void {
				footerBox.setStyle("verticalAlign", "middle");			
		}
		
	}
}