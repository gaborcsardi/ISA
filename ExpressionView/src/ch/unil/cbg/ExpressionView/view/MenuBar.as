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
		private var navigationBox:HBox;
		private var selectionBox:HBox;
		private var panelBox:HBox;
		private var windowBox:HBox;
		
		[Embed(source='/ch/unil/cbg/ExpressionView/assets/menu/inspect.png')]
		public var inspectIcon:Class; 
		[Embed(source='/ch/unil/cbg/ExpressionView/assets/menu/zoom.png')]
		public var zoomIcon:Class; 
		[Embed(source='/ch/unil/cbg/ExpressionView/assets/menu/pan.png')]
		public var panIcon:Class; 
		
		private var openButton:Button;
		private var navigationMenu:ToggleButtonBar;
		private var outlineVisibility:CheckBox;
		private var fillingVisibility:CheckBox;
		private var alphaSlider:HSlider;
		private var pdfExportButton:Button;
		private var gedatainfoVisibilityButton:Button;
		private var infoVisibilityButton:Button;
		private var modulesVisibilityButton:Button;
		private var genesVisibilityButton:Button;
		private var samplesVisibilityButton:Button;
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
		
		// alpha slider
		private function pdfExportHandler(event:MouseEvent): void {
			dispatchEvent(new MenuEvent(MenuEvent.PDF_EXPORT));
		}


		// set Panel visibility
		private function updatePanelVisibility(event:MenuEvent):void {
			var whichPanel:int = event.data[0];
			var visibility:Boolean = event.data[1];
			if ( whichPanel == 0 ) { gedatainfoVisibilityButton.selected = visibility; }
			else if ( whichPanel == 1 ) { infoVisibilityButton.selected = visibility; }
			else if ( whichPanel == 2 ) { modulesVisibilityButton.selected = visibility; }
			else if ( whichPanel == 3 ) { genesVisibilityButton.selected = visibility; }
			else if ( whichPanel == 4 ) { samplesVisibilityButton.selected = visibility; } 
		}

		
		private function gedatainfoVisibilityButtonClickHandler(event:MouseEvent): void {
			var visibility:Boolean = true;
			if ( !gedatainfoVisibilityButton.selected ) {
				visibility = false;
			}
			dispatchEvent(new MenuEvent(MenuEvent.PANELS, [0, visibility]));
		}
		private function infoVisibilityButtonClickHandler(event:MouseEvent): void {
			var visibility:Boolean = true;
			if ( !infoVisibilityButton.selected ) {
				visibility = false;
			}
			dispatchEvent(new MenuEvent(MenuEvent.PANELS, [1, visibility]));
		}
		private function modulesVisibilityButtonClickHandler(event:MouseEvent): void {
			var visibility:Boolean = true;
			if ( !modulesVisibilityButton.selected ) {
				visibility = false;
			}
			dispatchEvent(new MenuEvent(MenuEvent.PANELS, [2, visibility]));
		}
		private function genesVisibilityButtonClickHandler(event:MouseEvent): void {
			var visibility:Boolean = true;
			if ( !genesVisibilityButton.selected ) {
				visibility = false;
			}
			dispatchEvent(new MenuEvent(MenuEvent.PANELS, [3, visibility]));
		}
		private function samplesVisibilityButtonClickHandler(event:MouseEvent): void {
			var visibility:Boolean = true;
			if ( !samplesVisibilityButton.selected ) {
				visibility = false;
			}
			dispatchEvent(new MenuEvent(MenuEvent.PANELS, [4, visibility]));
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

				if ( !pdfExportButton ) {
					pdfExportButton = new Button();
					pdfExportButton.label = "PDF";
					pdfExportButton.toolTip = "Export PDF file.";
					pdfExportButton.styleName = "pdfExportButton";
					
					pdfExportButton.addEventListener(MouseEvent.CLICK, pdfExportHandler);
					fileBox.addChild(pdfExportButton);
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
					alphaSlider.value = 0.1;
					alphaSlider.addEventListener(SliderEvent.CHANGE, alphaSliderChangeHandler);
					selectionBox.addChild(alphaSlider);
				}
			}

			if ( !panelBox ) {
				panelBox = new HBox();
				panelBox.styleName = "menuItemBox";
				menuBox.addChild(panelBox);

				if ( !gedatainfoVisibilityButton ) {
					gedatainfoVisibilityButton = new Button();
					//gedatainfoVisibilityButton.label = "Data Description";
					gedatainfoVisibilityButton.selected = false;
					gedatainfoVisibilityButton.toggle = true;
					gedatainfoVisibilityButton.styleName = "gedatainfoVisibilityButton";
					gedatainfoVisibilityButton.toolTip = "Show/hide gene expression data description.";
					gedatainfoVisibilityButton.addEventListener(MouseEvent.CLICK, gedatainfoVisibilityButtonClickHandler);
					panelBox.addChild(gedatainfoVisibilityButton);
				}
				if ( !infoVisibilityButton ) {
					infoVisibilityButton = new Button();
					//infoVisibilityButton.label = "Info";
					infoVisibilityButton.selected = true;
					infoVisibilityButton.toggle = true;
					infoVisibilityButton.styleName = "infoVisibilityButton";
					infoVisibilityButton.toolTip = "Show/hide info panel.";
					infoVisibilityButton.addEventListener(MouseEvent.CLICK, infoVisibilityButtonClickHandler);
					panelBox.addChild(infoVisibilityButton);
				}
				if ( !modulesVisibilityButton ) {
					modulesVisibilityButton = new Button();
					//modulesVisibilityButton.label = "Modules";
					modulesVisibilityButton.selected = true;
					modulesVisibilityButton.toggle = true;
					modulesVisibilityButton.styleName = "modulesVisibilityButton";
					modulesVisibilityButton.toolTip = "Show/hide modules panel.";
					modulesVisibilityButton.addEventListener(MouseEvent.CLICK, modulesVisibilityButtonClickHandler);
					panelBox.addChild(modulesVisibilityButton);
				}
				if ( !genesVisibilityButton ) {
					genesVisibilityButton = new Button();
					//genesVisibilityButton.label = "Genes";
					genesVisibilityButton.selected = true;
					genesVisibilityButton.toggle = true;
					genesVisibilityButton.styleName = "genesVisibilityButton";
					genesVisibilityButton.toolTip = "Show/hide genes panel.";
					genesVisibilityButton.addEventListener(MouseEvent.CLICK, genesVisibilityButtonClickHandler);
					panelBox.addChild(genesVisibilityButton);
				}
				if ( !samplesVisibilityButton ) {
					samplesVisibilityButton = new Button();
					//samplesVisibilityButton.label = "Samples";
					samplesVisibilityButton.selected = true;
					samplesVisibilityButton.toggle = true;
					samplesVisibilityButton.styleName = "samplesVisibilityButton";
					samplesVisibilityButton.toolTip = "Show/hide samples panel.";
					samplesVisibilityButton.addEventListener(MouseEvent.CLICK, samplesVisibilityButtonClickHandler);
					panelBox.addChild(samplesVisibilityButton);
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
				parentApplication.addEventListener(MenuEvent.PANELS, updatePanelVisibility);
			
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