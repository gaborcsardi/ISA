package ch.unil.cbg.ExpressionView.view {
		
	import ch.unil.cbg.ExpressionView.events.*;
	import ch.unil.cbg.ExpressionView.model.*;
	
	import flash.events.Event;
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
		
		private var headerBox:HBox;
		
		private var openButton:Button;
		private var navigationMenu:ToggleButtonBar;
		private var outlineVisibility:CheckBox;
		private var fillingVisibility:CheckBox;
		private var alphaSlider:HSlider;
		private var pdfExportButton:Button;
		private var infoVisibilityButton:Button;
		private var modulesVisibilityButton:Button;
		private var genesVisibilityButton:Button;
		private var samplesVisibilityButton:Button;
		private var resizeButton:Button;
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
        	dispatchEvent(new ProcessFileEvent(event.target.data));
			file.removeEventListener(ProgressEvent.PROGRESS, fileLoadProgressHandler);
        	dispatchEvent(new UpdateStatusBarEvent("")); 					
			file.removeEventListener(Event.COMPLETE, fileLoadHandler);								
		}
		
		private function fileLoadProgressHandler(event:ProgressEvent):void {
			var status:String = "loading " + file.name + "..." + int(event.bytesLoaded/event.bytesTotal*100) + "% done";
        	dispatchEvent(new UpdateStatusBarEvent(status)); 		
		}			
		
		
		// inspect, zoom, and pan
		private function navigationMenuClickHandler(event:ItemClickEvent): void {
			dispatchEvent(new ModeChangeEvent(event.index));
		}
		
		// checkbox for outlines
		private function outlineVisibilityChangeHandler(event:MouseEvent): void {
			dispatchEvent(new SetOutlineVisibilityEvent(outlineVisibility.selected));
		}
		
		// checkbox for fillings
		private function fillingVisibilityChangeHandler(event:MouseEvent): void {
			dispatchEvent(new SetFillingVisibilityEvent(fillingVisibility.selected));
		}

		// alpha slider
		private function alphaSliderChangeHandler(event:SliderEvent): void {
			dispatchEvent(new AlphaSliderChangeEvent(event.value));
		}
		
		// alpha slider
		private function pdfExportHandler(event:MouseEvent): void {
			dispatchEvent(new PDFExportEvent());
		}


		// set Panel visibility
		private function infoVisibilityButtonClickHandler(event:MouseEvent): void {
			var visibility:Boolean = true;
			if ( !infoVisibilityButton.selected ) {
				visibility = false;
			}
			dispatchEvent(new SetPanelVisibilityEvent("info", visibility));
		}
		private function modulesVisibilityButtonClickHandler(event:MouseEvent): void {
			var visibility:Boolean = true;
			if ( !modulesVisibilityButton.selected ) {
				visibility = false;
			}
			dispatchEvent(new SetPanelVisibilityEvent("modules", visibility));
		}
		private function genesVisibilityButtonClickHandler(event:MouseEvent): void {
			var visibility:Boolean = true;
			if ( !genesVisibilityButton.selected ) {
				visibility = false;
			}
			dispatchEvent(new SetPanelVisibilityEvent("genes", visibility));
		}
		private function samplesVisibilityButtonClickHandler(event:MouseEvent): void {
			var visibility:Boolean = true;
			if ( !samplesVisibilityButton.selected ) {
				visibility = false;
			}
			dispatchEvent(new SetPanelVisibilityEvent("samples", visibility));
		}
		
		// resize
		private function resizeHandler(event:MouseEvent): void {
			dispatchEvent(new SetDefaultPositionsEvent());
		}
		// fullscreen
		private function fullScreenHandler(event:MouseEvent): void {
			var screen:String = "normal";
			if ( !fullScreenButton.selected ) {
				screen = "full";
			}
			dispatchEvent(new ToggleFullScreenEvent(screen));
		}
		
		// layout	
		override protected function createChildren() : void{	
			super.createChildren();
		
			if ( !headerBox ) {
				headerBox = new HBox();
				headerBox.setStyle("verticalAlign", "middle");
				
				addChild(headerBox);
				
				if ( !openButton ) {
					openButton = new Button();
					openButton.label = "Open";
					openButton.addEventListener(MouseEvent.CLICK, fileOpenHandler);
					headerBox.addChild(openButton);
				}

				if ( !navigationMenu ) {
					navigationMenu = new ToggleButtonBar();
					navigationMenu.dataProvider = new Array("Inspect", "Zoom", "Pan");
					navigationMenu.addEventListener(ItemClickEvent.ITEM_CLICK, navigationMenuClickHandler);
					headerBox.addChild(navigationMenu);
				}
				
				if ( !outlineVisibility ) {
					outlineVisibility = new CheckBox();
					outlineVisibility.selected = true;
					outlineVisibility.labelPlacement = "top";
					outlineVisibility.label = "Outline";
					outlineVisibility.addEventListener(MouseEvent.CLICK, outlineVisibilityChangeHandler);
					headerBox.addChild(outlineVisibility);
				}

				if ( !fillingVisibility ) {
					fillingVisibility = new CheckBox();
					fillingVisibility.selected = true;
					fillingVisibility.labelPlacement = "top";
					fillingVisibility.label = "Filling";
					fillingVisibility.addEventListener(MouseEvent.CLICK, fillingVisibilityChangeHandler);
					headerBox.addChild(fillingVisibility);
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
					headerBox.addChild(alphaSlider);
				}

				if ( !pdfExportButton ) {
					pdfExportButton = new Button();
					pdfExportButton.label = "PDF Export";
					pdfExportButton.addEventListener(MouseEvent.CLICK, pdfExportHandler);
					headerBox.addChild(pdfExportButton);
				}

				if ( !infoVisibilityButton ) {
					infoVisibilityButton = new Button();
					infoVisibilityButton.label = "Info";
					infoVisibilityButton.selected = true;
					infoVisibilityButton.toggle = true;
					infoVisibilityButton.addEventListener(MouseEvent.CLICK, infoVisibilityButtonClickHandler);
					headerBox.addChild(infoVisibilityButton);
				}
				if ( !modulesVisibilityButton ) {
					modulesVisibilityButton = new Button();
					modulesVisibilityButton.label = "Modules";
					modulesVisibilityButton.selected = true;
					modulesVisibilityButton.toggle = true;
					modulesVisibilityButton.addEventListener(MouseEvent.CLICK, modulesVisibilityButtonClickHandler);
					headerBox.addChild(modulesVisibilityButton);
				}
				if ( !genesVisibilityButton ) {
					genesVisibilityButton = new Button();
					genesVisibilityButton.label = "Genes";
					genesVisibilityButton.selected = true;
					genesVisibilityButton.toggle = true;
					genesVisibilityButton.addEventListener(MouseEvent.CLICK, genesVisibilityButtonClickHandler);
					headerBox.addChild(genesVisibilityButton);
				}
				if ( !samplesVisibilityButton ) {
					samplesVisibilityButton = new Button();
					samplesVisibilityButton.label = "Samples";
					samplesVisibilityButton.selected = true;
					samplesVisibilityButton.toggle = true;
					samplesVisibilityButton.addEventListener(MouseEvent.CLICK, samplesVisibilityButtonClickHandler);
					headerBox.addChild(samplesVisibilityButton);
				}

				if ( !resizeButton ) {
					resizeButton = new Button();
					resizeButton.label = "Default Positions";
					resizeButton.addEventListener(MouseEvent.CLICK, resizeHandler);
					headerBox.addChild(resizeButton);
				}
				if ( !fullScreenButton ) {
					fullScreenButton = new Button();
					fullScreenButton.label = "FullScreen";
					fullScreenButton.toggle = true;
					fullScreenButton.selected = false;
					fullScreenButton.addEventListener(MouseEvent.CLICK, fullScreenHandler);
					headerBox.addChild(fullScreenButton);
				}
			}
			
		}
		
		override protected function updateDisplayList(unscaledWidth:Number, unscaledHeight:Number):void {		
			super.updateDisplayList(unscaledWidth, unscaledHeight);
			headerBox.percentWidth = 100;
			headerBox.percentHeight = 100;
			alphaSlider.percentHeight = 100;
			alphaSlider.width = 120; 
		}
		
	}
}