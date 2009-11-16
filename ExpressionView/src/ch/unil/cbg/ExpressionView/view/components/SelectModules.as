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

package ch.unil.cbg.ExpressionView.view.components {
	
	import ch.unil.cbg.ExpressionView.events.PopUpEvent;
	
	import flash.events.MouseEvent;
	
	import mx.containers.HBox;
	import mx.containers.Panel;
	import mx.controls.Button;
	import mx.controls.Text;
	import mx.events.StateChangeEvent;

	[Event(name=PopUpEvent.CANCEL, type="ch.unil.cbg.expressionview.events.PopUpEvent")];
	[Event(name=PopUpEvent.OK, type="ch.unil.cbg.expressionview.events.PopUpEvent")];
	public class SelectModules extends Panel {
		
		private var modulesList:Array;
		private var textField:Text;
		private var modulesBar:HBox;
		private var buttonBar:HBox;
		private var modulesButtons:Array;
		private var selectNoneButton:Button;
		private var selectAllButton:Button;
		private var okButton:Button;
		private var cancelButton:Button;
				
		public function SelectModules(modules:Array) {
			super();
			this.styleName = "selectModulesPanel";
			modulesList = modules;
		}
		
		override protected function createChildren():void {
			super.createChildren();

			if ( !textField ) {
				textField = new Text();
				textField.text = "Which modules do you want to open?";
				addChild(textField);		
			}
			
			if ( !modulesBar ) {
				modulesBar = new HBox();
				addChild(modulesBar);
				
				modulesButtons = [];
				for ( var i:int = 0; i < modulesList.length; ++i ) {
					var button:Button = new Button();
					button.label = "m" + modulesList[i];
					button.stickyHighlighting = true;
					button.toggle = true;
					button.addEventListener(MouseEvent.CLICK, moduleButtonClickHandler);
					modulesButtons.push(button);
					modulesBar.addChild(button);
				}
			}

			if ( !buttonBar ) {
				buttonBar = new HBox();
				addChild(buttonBar);

				if ( !selectNoneButton ) {			
					selectNoneButton = new Button();
					selectNoneButton.label = "Select None";
					selectNoneButton.addEventListener(MouseEvent.CLICK, selectNoneButtonClickHandler);
					buttonBar.addChild(selectNoneButton);
				}
	
				if ( !selectAllButton ) {			
					selectAllButton = new Button();
					selectAllButton.label = "Select all";
					selectAllButton.addEventListener(MouseEvent.CLICK, selectAllButtonClickHandler);
					buttonBar.addChild(selectAllButton);
				}
	
				if ( !okButton ) {			
					okButton = new Button();
					okButton.label = "Ok";
					okButton.addEventListener(MouseEvent.CLICK, okButtonClickHandler);
					buttonBar.addChild(okButton);
				}
	
				if ( !cancelButton ) {			
					cancelButton = new Button();
					cancelButton.label = "Cancel";
					cancelButton.addEventListener(MouseEvent.CLICK, cancelButtonClickHandler);
					buttonBar.addChild(cancelButton);
				}
			}
		}

		override protected function updateDisplayList(unscaledWidth:Number, unscaledHeight:Number):void{			
			super.updateDisplayList(unscaledWidth, unscaledHeight);
			
			textField.x = 10;
			textField.y = 10;
			modulesBar.x = 10;
			modulesBar.y = textField.y + textField.measuredHeight + 10;
			buttonBar.x = 10;
			buttonBar.y = modulesBar.y + modulesBar.measuredHeight + 30;
			width = Math.max(textField.measuredWidth, modulesBar.measuredWidth, buttonBar.measuredWidth) + 20;
			height = buttonBar.y + buttonBar.measuredHeight + 50;
		}
		
		private function moduleButtonClickHandler(event:MouseEvent): void {
			if ( event.target.styleName == null ) {
				event.target.styleName = "selectedButton";
			} else {
				event.target.styleName = null;
			}
		}
		
		private function selectAllButtonClickHandler(event:MouseEvent): void {
			for ( var i:int = 0; i < modulesButtons.length; ++i ) {
				modulesButtons[i].selected = true;
				modulesButtons[i].styleName = "selectedButton";
			}
		}

		private function selectNoneButtonClickHandler(event:MouseEvent): void {
			for ( var i:int = 0; i < modulesButtons.length; ++i ) {
				modulesButtons[i].selected = false;
				modulesButtons[i].styleName = null;
			}
		}

		private function okButtonClickHandler(event:MouseEvent): void {
			var selection:Array = [];
			for ( var i:int = 0; i < modulesButtons.length; ++i ) {
				if ( modulesButtons[i].selected ) {
					selection.push(modulesList[i]);
				}
			}
			dispatchEvent(new PopUpEvent(PopUpEvent.OK, [selection]));
		}

		private function cancelButtonClickHandler(event:MouseEvent): void {
			dispatchEvent(new PopUpEvent(PopUpEvent.CANCEL));
		}
				
	}
}