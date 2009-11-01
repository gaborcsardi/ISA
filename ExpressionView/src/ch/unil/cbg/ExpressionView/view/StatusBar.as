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