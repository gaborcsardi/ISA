package ch.unil.cbg.ExpressionView.view.components {
	
	import ch.unil.cbg.ExpressionView.events.ClosableTabNavigatorEvent;
	
	import flash.display.DisplayObject;
	import flash.events.Event;
	
	import mx.controls.TabBar;
	import mx.core.ClassFactory;
	import mx.core.IFlexDisplayObject;
	import mx.core.mx_internal;
	
	public class ClosableTabBar extends TabBar {
		
		use namespace mx_internal;
		
		public function ClosableTabBar() {
			super();
			navItemFactory = new ClassFactory(ClosableTab);
		}
		
		override protected function createChildren():void {
			super.createChildren();
		}
		
		override protected function createNavItem(label:String, icon:Class = null):IFlexDisplayObject{
			var tab:ClosableTab = super.createNavItem(label,icon) as ClosableTab;
			tab.addEventListener(ClosableTab.CLOSE_TAB_EVENT, closeTabClickHandler);
			return tab;
		}
		
		public function set enableClose(data:Array):void {
			if ( data[0] < numChildren ) {
				var child:ClosableTab = ClosableTab(getChildAt(data[0]));
				child.enableClose = data[1];
			}
		}
		
		private function closeTabClickHandler(event:Event):void{
			var index:int = getChildIndex(DisplayObject(event.currentTarget));
			dispatchEvent(new ClosableTabNavigatorEvent(ClosableTabNavigatorEvent.CLOSE, [index]));
		}
		
	}
}