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
	import ch.unil.cbg.ExpressionView.model.*;
	
	import flash.events.Event;
	import flash.events.KeyboardEvent;
	import flash.events.MouseEvent;
	import flash.events.ProgressEvent;
	import flash.net.FileFilter;
	import flash.net.FileReference;
	
	import mx.containers.Canvas;
	import mx.containers.HBox;
	import mx.controls.Button;
	import mx.controls.CheckBox;
	import mx.controls.HSlider;
	import mx.controls.ToggleButtonBar;
	import mx.events.ItemClickEvent;
	import mx.events.SliderEvent;

	public class MenuBar extends Canvas {
		
		private var menuBox:HBox;
		private var fileBox:HBox;
		private var exportBox:HBox;
		private var navigationBox:HBox;
		private var selectionBox:HBox;
		private var windowBox:HBox;
		
		[Embed(source='/ch/unil/cbg/ExpressionView/assets/menu/inspect.png')]
		public var inspectIcon:Class; 
		[Embed(source='/ch/unil/cbg/ExpressionView/assets/menu/zoom.png')]
		public var zoomIcon:Class; 
		[Embed(source='/ch/unil/cbg/ExpressionView/assets/menu/pan.png')]
		public var panIcon:Class; 
		
		private var openButton:Button;
		private var pdfExportButton:Button;
		private var excelExportButton:Button;
		private var navigationMenu:ToggleButtonBar;
		private var outlineVisibility:CheckBox;
		private var fillingVisibility:CheckBox;
		private var alphaSlider:HSlider;
		private var defaultPositionsButton:Button;
		private var fullScreenButton:Button;

		private var file:FileReference = new FileReference();

		public function MenuBar() {
			super();
		}
				
		// open
		private function fileOpenHandler(event:MouseEvent):void {
			var filter:FileFilter = new FileFilter("Gene Expression Data", "*");
			file.browse([filter]);
            file.addEventListener(Event.SELECT, fileSelectHandler);
			file.addEventListener(ProgressEvent.PROGRESS, fileLoadProgressHandler);				
			file.addEventListener(Event.COMPLETE, fileLoadHandler);
        }
        
        private function fileSelectHandler(event:Event) : void {
			file.load();
			file.removeEventListener(Event.SELECT, fileSelectHandler);
		}
        
        private function fileLoadHandler(event:Event):void {
			dispatchEvent(new MenuEvent(MenuEvent.OPEN, [event.target.data]));        	
			file.removeEventListener(ProgressEvent.PROGRESS, fileLoadProgressHandler);
        	dispatchEvent(new UpdateStatusBarEvent("")); 					
			file.removeEventListener(Event.COMPLETE, fileLoadHandler);								
		}
		
		private function fileLoadProgressHandler(event:ProgressEvent):void {
			var status:String = "loading " + file.name + "..." + int(event.bytesLoaded/event.bytesTotal*100) + "% done";
        	dispatchEvent(new UpdateStatusBarEvent(status)); 		
		}			
		
		
		// inspect, zoom, and pan
		private function keyHandler(event:KeyboardEvent): void {
			// i = 73, z = 89, and p = 80;
			if ( event.keyCode == 73 ) {
				dispatchEvent(new MenuEvent(MenuEvent.MODE, [0]))
				navigationMenu.selectedIndex = 0;
			} else if ( event.keyCode == 89 ) {
				dispatchEvent(new MenuEvent(MenuEvent.MODE, [1]))
				navigationMenu.selectedIndex = 1;
			}
			if ( event.keyCode == 80 ) {
				dispatchEvent(new MenuEvent(MenuEvent.MODE, [2]))
				navigationMenu.selectedIndex = 2;
			}
		}
		private function navigationMenuClickHandler(event:ItemClickEvent): void {
			dispatchEvent(new MenuEvent(MenuEvent.MODE, [event.index]));
		}
		
		// checkbox for outlines
		private function outlineVisibilityChangeHandler(event:MouseEvent): void {
			dispatchEvent(new MenuEvent(MenuEvent.OUTLINE, [outlineVisibility.selected]));
		}
		
		// checkbox for fillings
		private function fillingVisibilityChangeHandler(event:MouseEvent): void {
			dispatchEvent(new MenuEvent(MenuEvent.FILLING, [fillingVisibility.selected]));
		}

		// alpha slider
		private function alphaSliderChangeHandler(event:SliderEvent): void {
			dispatchEvent(new MenuEvent(MenuEvent.ALPHA, [event.value]));
		}
		
		// pdf export
		private function pdfExportHandler(event:MouseEvent): void {
			dispatchEvent(new MenuEvent(MenuEvent.PDF_EXPORT));
		}
		// excel export
		private function excelExportHandler(event:MouseEvent): void {
			dispatchEvent(new MenuEvent(MenuEvent.EXCEL_EXPORT));
		}
				
		// resize
		private function resizeHandler(event:MouseEvent): void {
			dispatchEvent(new MenuEvent(MenuEvent.DEFAULT_POSITIONS));
		}
		// fullscreen
		private function fullScreenHandler(event:MouseEvent): void {
			var screen:String = "normal";
			if ( !fullScreenButton.selected ) {
				screen = "full";
			}
			dispatchEvent(new MenuEvent(MenuEvent.FULLSCREEN, [screen]));
		}
		
		// layout	
		override protected function createChildren() : void{	
			super.createChildren();
			
			if ( !menuBox ) {
				menuBox = new HBox;
				menuBox.styleName = "menuBox";
				addChild(menuBox);
			}
			
			if ( !fileBox ) {
				fileBox = new HBox();
				fileBox.styleName = "menuItemBox";
				menuBox.addChild(fileBox);
				
				if ( !openButton ) {
					openButton = new Button();
					openButton.label = "Open";
					openButton.styleName = "openButton";
					openButton.toolTip = "Open ExpressionView file.";
					openButton.addEventListener(MouseEvent.CLICK, fileOpenHandler);
					fileBox.addChild(openButton);
				}
			}
			
			if ( !exportBox ) {
				exportBox = new HBox();
				exportBox.styleName = "menuItemBox";
				menuBox.addChild(exportBox);
				
				if ( !pdfExportButton ) {
					pdfExportButton = new Button();
					pdfExportButton.label = "PDF";
					pdfExportButton.toolTip = "Export PDF file of currently viewed area.";
					pdfExportButton.styleName = "pdfExportButton";
					
					pdfExportButton.addEventListener(MouseEvent.CLICK, pdfExportHandler);
					exportBox.addChild(pdfExportButton);
				}
				
				if ( !excelExportButton ) {
					excelExportButton = new Button();
					excelExportButton.label = "CSV";
					excelExportButton.toolTip = "Export Excel (CSV) file containing all the information\n associated with the currently viewed module.";
					excelExportButton.styleName = "excelExportButton";
					
					excelExportButton.addEventListener(MouseEvent.CLICK, excelExportHandler);
					exportBox.addChild(excelExportButton);
				}
			}

			if ( !navigationBox ) {
				navigationBox = new HBox();
				navigationBox.styleName = "menuItemBox";
				menuBox.addChild(navigationBox);

				if ( !navigationMenu ) {
					navigationMenu = new ToggleButtonBar();
					var buttons:Array = [];
					var item:Object = new Object();
					item.label = "Inspect";
					item.icon = inspectIcon;
					item.toolTip = "Inspect";
					buttons.push(item);
					item = new Object();
					item.label = "Zoom";
					item.icon = zoomIcon;
					item.toolTip = "Zoom";
					buttons.push(item);
					item = new Object();
					item.label = "Pan";
					item.icon = panIcon;
					item.toolTip = "Pan";
					buttons.push(item);
					navigationMenu.dataProvider = buttons;
					navigationMenu.addEventListener(ItemClickEvent.ITEM_CLICK, navigationMenuClickHandler);
					navigationMenu.styleName = "navigationMenu";
					navigationBox.addChild(navigationMenu);
				}
			}
			
			if ( !selectionBox ) {
				selectionBox = new HBox();
				selectionBox.styleName = "menuSpecialItemBox";
				menuBox.addChild(selectionBox);

				if ( !outlineVisibility ) {
					outlineVisibility = new CheckBox();
					outlineVisibility.selected = true;
					outlineVisibility.labelPlacement = "top";
					outlineVisibility.label = "Outline";
					outlineVisibility.addEventListener(MouseEvent.CLICK, outlineVisibilityChangeHandler);
					selectionBox.addChild(outlineVisibility);
				}

				if ( !fillingVisibility ) {
					fillingVisibility = new CheckBox();
					fillingVisibility.selected = true;
					fillingVisibility.labelPlacement = "top";
					fillingVisibility.label = "Filling";
					fillingVisibility.addEventListener(MouseEvent.CLICK, fillingVisibilityChangeHandler);
					selectionBox.addChild(fillingVisibility);
				}

				if ( !alphaSlider ) {
					alphaSlider = new HSlider();
					alphaSlider.minimum = 0;
					alphaSlider.maximum = 1;
					alphaSlider.tickInterval = 1;
					alphaSlider.snapInterval = 0.1;
					alphaSlider.labels = ["Modules", "GE Data"];
					alphaSlider.tickValues = [0, 1]
					alphaSlider.liveDragging = true;
					alphaSlider.value = 0.2;
					alphaSlider.addEventListener(SliderEvent.CHANGE, alphaSliderChangeHandler);
					selectionBox.addChild(alphaSlider);
				}
			}

			if ( !windowBox ) {
				windowBox = new HBox();
				windowBox.styleName = "menuItemBox";
				menuBox.addChild(windowBox);				

				if ( !defaultPositionsButton ) {
					defaultPositionsButton = new Button();
					defaultPositionsButton.label = "Default";
					defaultPositionsButton.styleName = "defaultPositionsButton";
					defaultPositionsButton.toolTip = "Align panels at default positions.";
					defaultPositionsButton.addEventListener(MouseEvent.CLICK, resizeHandler);
					windowBox.addChild(defaultPositionsButton);
				}
				if ( !fullScreenButton ) {
					fullScreenButton = new Button();
					fullScreenButton.label = "Fullscreen";
					fullScreenButton.toggle = true;
					fullScreenButton.selected = false;
					fullScreenButton.styleName = "fullScreenButton";
					fullScreenButton.toolTip = "Change to fullscreen view.";
					fullScreenButton.addEventListener(MouseEvent.CLICK, fullScreenHandler);
					windowBox.addChild(fullScreenButton);
				}
				
				parentApplication.addEventListener(KeyboardEvent.KEY_UP, keyHandler);
				
			}
			
		}
		
		override protected function updateDisplayList(unscaledWidth:Number, unscaledHeight:Number):void {		
			super.updateDisplayList(unscaledWidth, unscaledHeight);

			menuBox.percentWidth = 100;
			menuBox.percentHeight = 100;
			
			alphaSlider.percentHeight = 100;
			alphaSlider.width = 120;
			 
		}
		
	}
}