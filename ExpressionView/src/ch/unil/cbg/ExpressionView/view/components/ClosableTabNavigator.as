package ch.unil.cbg.ExpressionView.view.components {
	
	import mx.containers.TabNavigator;
	import mx.controls.Button;

	public class ClosableTabNavigator extends TabNavigator {
		
		public var enableClose:Boolean;
		
		private var closeButton:Button;
		
		public function ClosableTabNavigator() {
			super();
			enableClose = true;
		}
		
		override protected function createChildren():void {
			
			// override tabBar before it gets created in the superclass
			if ( !tabBar ) {
				tabBar = new ClosableTabBar(); 
				rawChildren.addChild(tabBar);		
			}

			super.createChildren();

		}
		
		
		
	}
}