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
	import flash.net.URLRequest;
	import flash.net.navigateToURL;
	
	import mx.containers.Canvas;
	import mx.containers.HBox;
	import mx.containers.VBox;
	import mx.controls.Button;
	import mx.controls.CheckBox;
	import mx.controls.HSlider;
	import mx.controls.Image;
	import mx.controls.Text;
	import mx.controls.ToggleButtonBar;
	import mx.events.ItemClickEvent;
	import mx.events.SliderEvent;

	public class MenuBar extends Canvas {
		
		private var menuBox:HBox;
		private var fileBox:VBox;
		private var fileBoxP:HBox;
		private var fileBoxTitle:Text;
		private var exportBox:VBox;
		private var exportBoxP:HBox;
		private var exportBoxTitle:Text;
		private var navigationBox:VBox;
		private var navigationBoxP:HBox;
		private var navigationBoxTitle:Text;
		private var viewBox:VBox;
		private var viewBoxP:HBox;
		private var viewBoxTitle:Text
		private var windowBox:VBox;
		private var windowBoxP:HBox;
		private var windowBoxTitle:Text;
		private var helpBox:VBox;
		private var helpBoxP:HBox;
		private var helpBoxTitle:Text;
		
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
		private var highlightingVisibility:CheckBox;
		private var outlineVisibility:CheckBox;
		private var fillingVisibility:CheckBox;
		private var alphaSlider:HSlider;
		private var defaultPositionsButton:Button;
		private var fullScreenButton:Button;
		private var websiteButton:Button;

		private var file:FileReference = new FileReference();

		public function MenuBar() {
			super();
			super.styleName = "menuBox";
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
			var focus:String = getFocus().name;
			if ( focus.search("TextField") == -1 ) {
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
		}
		private function navigationMenuClickHandler(event:ItemClickEvent): void {
			dispatchEvent(new MenuEvent(MenuEvent.MODE, [event.index]));
		}

		// checkbox for highlighting
		private function highlightingVisibilityChangeHandler(event:MouseEvent): void {
			dispatchEvent(new MenuEvent(MenuEvent.HIGHLIGHTING, [highlightingVisibility.selected]));
			if ( event.shiftKey ) {
				dispatchEvent(new MenuEvent(MenuEvent.OUTLINE, [highlightingVisibility.selected]));
				dispatchEvent(new MenuEvent(MenuEvent.FILLING, [highlightingVisibility.selected]));
				outlineVisibility.selected = highlightingVisibility.selected;
				fillingVisibility.selected = highlightingVisibility.selected;
				if ( !highlightingVisibility.selected ) {
					alphaSlider.value = 1;
					dispatchEvent(new MenuEvent(MenuEvent.ALPHA, [alphaSlider.value]));
				} else {
					alphaSlider.value = 0.4;
					dispatchEvent(new MenuEvent(MenuEvent.ALPHA, [alphaSlider.value]));
				}
			}
		}
		
		// checkbox for outlines
		private function outlineVisibilityChangeHandler(event:MouseEvent): void {
			dispatchEvent(new MenuEvent(MenuEvent.OUTLINE, [outlineVisibility.selected]));
			if ( event.shiftKey ) {
				dispatchEvent(new MenuEvent(MenuEvent.HIGHLIGHTING, [outlineVisibility.selected]));
				dispatchEvent(new MenuEvent(MenuEvent.FILLING, [outlineVisibility.selected]));
				highlightingVisibility.selected = outlineVisibility.selected;
				fillingVisibility.selected = outlineVisibility.selected;
				if ( !outlineVisibility.selected ) {
					alphaSlider.value = 1;
					dispatchEvent(new MenuEvent(MenuEvent.ALPHA, [alphaSlider.value]));
				} else {
					alphaSlider.value = 0.4;
					dispatchEvent(new MenuEvent(MenuEvent.ALPHA, [alphaSlider.value]));
				}
			}
		}
		
		// checkbox for fillings
		private function fillingVisibilityChangeHandler(event:MouseEvent): void {
			dispatchEvent(new MenuEvent(MenuEvent.FILLING, [fillingVisibility.selected]));
			if ( event.shiftKey ) {
				dispatchEvent(new MenuEvent(MenuEvent.HIGHLIGHTING, [fillingVisibility.selected]));
				dispatchEvent(new MenuEvent(MenuEvent.OUTLINE, [fillingVisibility.selected]));
				highlightingVisibility.selected = fillingVisibility.selected;
				outlineVisibility.selected = fillingVisibility.selected;
				if ( !fillingVisibility.selected ) {
					alphaSlider.value = 1;
					dispatchEvent(new MenuEvent(MenuEvent.ALPHA, [alphaSlider.value]));
				} else {
					alphaSlider.value = 0.4;
					dispatchEvent(new MenuEvent(MenuEvent.ALPHA, [alphaSlider.value]));
				}
			}
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
		// fullscreen
		private function websiteHandler(event:MouseEvent): void {
			var urlRequest:URLRequest = new URLRequest("http://www2.unil.ch/cbg/index.php?title=ExpressionView");
			navigateToURL(urlRequest, String("_blank"))
		}
		
		// layout	
		override protected function createChildren() : void{	
			super.createChildren();
			
			if ( !menuBox ) {
				menuBox = new HBox();
				menuBox.styleName = "menuBox";
				addChild(menuBox);
			}
						
			// file
			if ( !fileBox ) {
				fileBox = new VBox();
				fileBox.styleName = "menuGroupBox";
				menuBox.addChild(fileBox);
								
				if ( !fileBoxTitle ) {
					fileBoxTitle = new Text();
					fileBoxTitle.text = "File";
					fileBox.addChild(fileBoxTitle);
				}
				
				if ( !fileBoxP ) {
					fileBoxP = new HBox;
					fileBoxP.styleName = "menuItemBox";
					fileBox.addChild(fileBoxP);
				
					if ( !openButton ) {
						openButton = new Button();
						openButton.label = "Open";
						openButton.styleName = "openButton";
						openButton.toolTip = "Open ExpressionView file.";
						openButton.addEventListener(MouseEvent.CLICK, fileOpenHandler);
						fileBoxP.addChild(openButton);
					}
				}
			}
			
			// export
			if ( !exportBox ) {
				exportBox = new VBox();
				exportBox.styleName = "menuGroupBox";
				menuBox.addChild(exportBox);
				
				if ( !exportBoxTitle ) {
					exportBoxTitle = new Text();
					exportBoxTitle.text = "Export";
					exportBox.addChild(exportBoxTitle);
				}
				
				if ( !exportBoxP ) { 
					exportBoxP = new HBox();
					exportBoxP.styleName = "menuItemBoxP";
					exportBox.addChild(exportBoxP);
				
					if ( !pdfExportButton ) {
						pdfExportButton = new Button();
						pdfExportButton.label = "PDF";
						pdfExportButton.toolTip = "Export PDF file of currently viewed area.";
						pdfExportButton.styleName = "pdfExportButton";
						
						pdfExportButton.addEventListener(MouseEvent.CLICK, pdfExportHandler);
						exportBoxP.addChild(pdfExportButton);
					}
					
					if ( !excelExportButton ) {
						excelExportButton = new Button();
						excelExportButton.label = "CSV";
						excelExportButton.toolTip = "Export Excel (CSV) file containing all the information\n associated with the currently viewed bicluster.";
						excelExportButton.styleName = "excelExportButton";
						
						excelExportButton.addEventListener(MouseEvent.CLICK, excelExportHandler);
						exportBoxP.addChild(excelExportButton);
					}
				}
			}

			// navigation
			if ( !navigationBox ) {
				navigationBox = new VBox();
				navigationBox.styleName = "menuGroupBox";
				menuBox.addChild(navigationBox);

				if ( !navigationBoxTitle ) {
					navigationBoxTitle = new Text();
					navigationBoxTitle.text = "Navigation";
					navigationBox.addChild(navigationBoxTitle);
				}

				if ( !navigationBoxP ) {
					navigationBoxP = new HBox();
					navigationBoxP.styleName = "menuItemBoxP";
					navigationBox.addChild(navigationBoxP);
					
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
						navigationBoxP.addChild(navigationMenu);
					}
				}
			}
			
			// view
			if ( !viewBox ) {
				viewBox = new VBox();
				viewBox.styleName = "menuGroupBox";
				menuBox.addChild(viewBox);

				if ( !viewBoxTitle ) {
					viewBoxTitle = new Text();
					viewBoxTitle.text = "View";
					viewBox.addChild(viewBoxTitle);
				}

				if ( !viewBoxP ) {
					viewBoxP = new HBox();
					viewBoxP.styleName = "menuItemBoxP";
					viewBox.addChild(viewBoxP);

					if ( !highlightingVisibility ) {
						highlightingVisibility = new CheckBox();
						highlightingVisibility.selected = true;
						highlightingVisibility.labelPlacement = "top";
						highlightingVisibility.label = "Highlighting";
						highlightingVisibility.toolTip = "Highlight biclusters under mouse pointer.";
						highlightingVisibility.addEventListener(MouseEvent.CLICK, highlightingVisibilityChangeHandler);
						viewBoxP.addChild(highlightingVisibility);
					}
	
					if ( !outlineVisibility ) {
						outlineVisibility = new CheckBox();
						outlineVisibility.selected = true;
						outlineVisibility.labelPlacement = "top";
						outlineVisibility.label = "Outline";
						outlineVisibility.toolTip = "Show outlines of the largest contiguous part of every bicluster.";
						outlineVisibility.addEventListener(MouseEvent.CLICK, outlineVisibilityChangeHandler);
						viewBoxP.addChild(outlineVisibility);
					}
	
					if ( !fillingVisibility ) {
						fillingVisibility = new CheckBox();
						fillingVisibility.selected = true;
						fillingVisibility.labelPlacement = "top";
						fillingVisibility.label = "Filling";
						fillingVisibility.toolTip = "Show biclusters.";
						fillingVisibility.addEventListener(MouseEvent.CLICK, fillingVisibilityChangeHandler);
						viewBoxP.addChild(fillingVisibility);
					}
	
					if ( !alphaSlider ) {
						alphaSlider = new HSlider();
						alphaSlider.minimum = 0;
						alphaSlider.maximum = 1;
						alphaSlider.tickInterval = 1;
						alphaSlider.snapInterval = 0.1;
						alphaSlider.labels = ["Biclusters", "Data"];
						alphaSlider.toolTip = "Choose to either focus on the biclusters (slide to the left) or on the underlying data (slide to the right)";
						alphaSlider.tickValues = [0, 1]
						alphaSlider.liveDragging = true;
						alphaSlider.value = 0.4;
						alphaSlider.addEventListener(SliderEvent.CHANGE, alphaSliderChangeHandler);
						viewBoxP.addChild(alphaSlider);
					}
				}
			}

			// window
			if ( !windowBox ) {
				windowBox = new VBox();
				windowBox.styleName = "menuGroupBox";
				menuBox.addChild(windowBox);				

				if ( !windowBoxTitle ) {
					windowBoxTitle = new Text();
					windowBoxTitle.text = "Window";
					windowBox.addChild(windowBoxTitle);
				}
				
				if ( !windowBoxP ) {
					windowBoxP = new HBox();
					windowBoxP.styleName = "menuItemBox";
					windowBox.addChild(windowBoxP);

					if ( !defaultPositionsButton ) {
						defaultPositionsButton = new Button();
						defaultPositionsButton.label = "Default";
						defaultPositionsButton.styleName = "defaultPositionsButton";
						defaultPositionsButton.toolTip = "Align panels at default positions.";
						defaultPositionsButton.addEventListener(MouseEvent.CLICK, resizeHandler);
						windowBoxP.addChild(defaultPositionsButton);
					}
					if ( !fullScreenButton ) {
						fullScreenButton = new Button();
						fullScreenButton.label = "Fullscreen";
						fullScreenButton.toggle = true;
						fullScreenButton.selected = false;
						fullScreenButton.styleName = "fullScreenButton";
						fullScreenButton.toolTip = "Change to fullscreen view.";
						fullScreenButton.addEventListener(MouseEvent.CLICK, fullScreenHandler);
						windowBoxP.addChild(fullScreenButton);
					}
				}
				
			}
			
			// help
			if ( !helpBox ) {
				helpBox = new VBox();
				helpBox.styleName = "menuGroupBox";
				menuBox.addChild(helpBox);				

				if ( !helpBoxTitle ) {
					helpBoxTitle = new Text();
					helpBoxTitle.text = "Help";
					helpBox.addChild(helpBoxTitle);
				}

				if ( !helpBoxP ) {
					helpBoxP = new HBox();
					helpBoxP.styleName = "menuItemBox";
					helpBox.addChild(helpBoxP);

					if ( !websiteButton ) {
						websiteButton = new Button();
						websiteButton.label = "Website";
						websiteButton.styleName = "websiteButton";
						websiteButton.toolTip = "Visit the ExpressionView website.";
						websiteButton.addEventListener(MouseEvent.CLICK, websiteHandler);
						helpBoxP.addChild(websiteButton);
					}
				}
			}
				
			parentApplication.addEventListener(KeyboardEvent.KEY_UP, keyHandler);

		}
		
		override protected function updateDisplayList(unscaledWidth:Number, unscaledHeight:Number):void {		
			super.updateDisplayList(unscaledWidth, unscaledHeight);

			//menuBox.x = 10;
			menuBox.percentWidth = 100;
			menuBox.percentHeight = 100;
									
			alphaSlider.percentHeight = 100;
			alphaSlider.width = 120;
			
		}
		
	}
}