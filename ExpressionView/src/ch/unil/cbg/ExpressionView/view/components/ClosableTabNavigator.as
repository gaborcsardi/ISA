// inspired by Doug McCune SuperTabNavigator (http://dougmccune.com/blog)

package ch.unil.cbg.ExpressionView.view.components {
	
	import mx.containers.HBox;
	import mx.containers.TabNavigator;
	import mx.controls.Button;

	public class ClosableTabNavigator extends TabNavigator {
		
		private var closeButton:Button;
		
		public function ClosableTabNavigator() {
			super();
		}
		
		override protected function createChildren():void {
			
			// override tabBar before it gets created in the superclass
			if ( !tabBar ) {
				tabBar = new ClosableTabBar();
				rawChildren.addChild(tabBar); 
			}

			super.createChildren();

		}

		public function set enableClose(data:Array):void {
	    	(tabBar as ClosableTabBar).enableClose = data;
    		invalidateDisplayList();
	    }
	    
	}
}