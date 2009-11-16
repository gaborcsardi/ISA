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