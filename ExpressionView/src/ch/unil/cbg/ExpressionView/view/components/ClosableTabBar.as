//     ExpressionView - A package to visualize biclusters
//     Copyright (C) 2009 Computational Biology Group, University of Lausanne
// 
//     This program is free software: you can redistribute it and/or modify
//     it under the terms of the GNU General Public License as published by
//     the Free Software Foundation, either version 3 of the License, or
//     (at your option) any later version.
// 
//     This program is distributed in the hope that it will be useful,
//     but WITHOUT ANY WARRANTY; without even the implied warranty of
//     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//     GNU General Public License for more details.
// 
//     You should have received a copy of the GNU General Public License
//     along with this program.  If not, see <http://www.gnu.org/licenses/>.

//     inspired by Doug McCune SuperTabNavigator (http://dougmccune.com/blog)

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