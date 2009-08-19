package ch.unil.cbg.ExpressionView.view.components {
	
	import mx.containers.TabNavigator;

	public class ClosableTabNavigator extends TabNavigator {
		
		public var enableClose:Boolean;
		
		public function ClosableTabNavigator() {
			super();
			enableClose = true;
		}
		
		override protected function createChildren():void {
			super.createChildren();
			
			
		}
		
	}
}