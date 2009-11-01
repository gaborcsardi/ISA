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
	
	import flash.events.Event;
	import flash.events.MouseEvent;
	
	import mx.controls.Button;
	import mx.controls.tabBarClasses.Tab;

	public class ClosableTab extends Tab {
		
		private var closeButton:Button;
		
		public static const ALWAYS:String = "always";
		public static const NEVER:String = "never";
		public static const ROLLOVER:String = "rollover";
		//public static const SELECTED:String = "selected";
		public static const CLOSE_TAB_EVENT:String = "closeTab";
		private var _enableClose:String = NEVER;
		
		private var overCloseButton:Boolean = false;
				
		public function ClosableTab() {
			super();
			mouseChildren = true;	
		}
		
		override protected function createChildren():void {
			super.createChildren();
			
			closeButton = new Button();
			closeButton.styleName = "tabCloseButton";
			closeButton.width = 10;
			closeButton.height = 10;
			closeButton.addEventListener(MouseEvent.CLICK, closeButtonClickHandler);
			addChild(closeButton);
		}

		override protected function updateDisplayList(unscaledWidth:Number, unscaledHeight:Number):void{			
			super.updateDisplayList(unscaledWidth, unscaledHeight);
			
			setChildIndex(closeButton, numChildren - 1);
			closeButton.visible = false;
			
			if ( _enableClose == ALWAYS ) {
				closeButton.visible = true;				
			}
			if ( overCloseButton && _enableClose == ROLLOVER ) {
				closeButton.visible = true;
			}
			
			if ( closeButton.visible ) {
				textField.width -= closeButton.width;
				textField.truncateToFit();

				closeButton.y = (unscaledHeight - closeButton.height) / 2;
				closeButton.x = unscaledWidth - closeButton.width - closeButton.y;
			}
		}
		
		public function set enableClose(value:String):void {
			_enableClose = value;
			invalidateDisplayList();
		}
		
		public function get enableClose():String {
			return _enableClose;
		}
		
		override protected function rollOverHandler(event:MouseEvent):void{
			overCloseButton = true;
			super.rollOverHandler(event);	
		}
		
		override protected function rollOutHandler(event:MouseEvent):void{
			overCloseButton = false;
			super.rollOutHandler(event);	
		}
		
		private function closeButtonClickHandler(event:MouseEvent):void {
			dispatchEvent(new Event(CLOSE_TAB_EVENT));
			event.stopImmediatePropagation();
		}
		
	}
}