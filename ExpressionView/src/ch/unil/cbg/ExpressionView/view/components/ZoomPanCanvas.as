package ch.unil.cbg.ExpressionView.view.components {
	
	import ch.unil.cbg.ExpressionView.events.*;
	import ch.unil.cbg.ExpressionView.model.*;
	import ch.unil.cbg.ExpressionView.utilities.LargeBitmapData;
	
	import flash.display.Bitmap;
	import flash.display.BitmapData;
	import flash.display.Shape;
	import flash.events.KeyboardEvent;
	import flash.events.MouseEvent;
	import flash.geom.ColorTransform;
	import flash.geom.Point;
	import flash.geom.Rectangle;
	import flash.utils.getTimer;
	
	import mx.containers.Canvas;
	import mx.controls.HScrollBar;
	import mx.controls.Image;
	import mx.controls.VScrollBar;
	import mx.events.ScrollEvent;


	public class ZoomPanCanvas extends Canvas {

		private const MINIMAL_WIDTH:int = 2;
		private const MINIMAL_HEIGHT:int = 2;
	
		private static const INSPECT:int = 0;
    	private static const ZOOM:int = 1;
    	private static const PAN:int = 2;
	
		private var fullgeimage:LargeBitmapData;
		private var fullmodulesimage:LargeBitmapData;
		private var maximalWidth:int = 0;
		private var maximalHeight:int = 0;
		
		private var currentgeimage:Bitmap;
		private var currentmodulesimage:Bitmap;		
				
		private var lastMode:int;
		
		private var lastPt:Point;
		private var currentRectangle:Rectangle;
		private var lastRectangle:Rectangle;
		private var selectionRectangle:Rectangle;
		private var selection:Shape;
		
		private var selectedGene:int;
		private var selectedSample:int;
		
		private var modulesOutlines:Array;
		private var modulesColors:Vector.<Array>;
		
		private var lastClick:int;
		
		private var highlightedModules:Array;
		private var highlightedGenes:Array;
		private var highlightedSamples:Array;
		
		
		// layout
		private var canvaswidth:Number;
		private var canvasheight:Number;
		private var hscrollbar:HScrollBar;
		private var vscrollbar:VScrollBar;		
		private var geimage:Image;
		private var modulesimage:Image;
		private var modulesCanvas:Canvas;
		private var highlightCanvas:Canvas;
		private var overlayCanvas:Canvas;
		
		public function ZoomPanCanvas() {
			super();
			lastRectangle = new Rectangle();
			lastPt = new Point();
			
			currentgeimage = new Bitmap();
			currentmodulesimage = new Bitmap();
						
			lastMode = 0;
			
			canvaswidth = this.width;
			canvasheight = this.height;
			
			highlightedModules = [];
			highlightedGenes = [];
			highlightedSamples = [];
		}
		
		private function modeChangeHandler(event:MenuEvent):void {
			var mode:int = event.data[0];
			if ( mode != lastMode ) {
				if ( lastMode == INSPECT ) {
					overlayCanvas.removeEventListener(MouseEvent.MOUSE_MOVE, inspectMouseMoveHandler);
					overlayCanvas.removeEventListener(MouseEvent.MOUSE_OUT, inspectMouseOutHandler);
					overlayCanvas.removeEventListener(MouseEvent.CLICK, inspectMouseClickHandler);
				} else if ( lastMode == ZOOM ) {
					overlayCanvas.removeEventListener(MouseEvent.MOUSE_DOWN, zoomMouseDownHandler);
					overlayCanvas.removeEventListener(MouseEvent.MOUSE_MOVE, zoomMouseMoveHandler);
					overlayCanvas.removeEventListener(MouseEvent.MOUSE_UP, zoomMouseUpHandler);
					overlayCanvas.removeEventListener(KeyboardEvent.KEY_UP, zoomKeyUpHandler);
					dispatchEvent(new UpdateStatusBarEvent("")); 
				} else if ( lastMode == PAN ) {
					overlayCanvas.removeEventListener(MouseEvent.MOUSE_UP, dragMouseUpHandler);
					overlayCanvas.removeEventListener(MouseEvent.MOUSE_MOVE, dragMouseMoveHandler);
					overlayCanvas.removeEventListener(MouseEvent.MOUSE_DOWN, dragMouseDownHandler);					
				}
				
				if ( mode == INSPECT ) {
					overlayCanvas.addEventListener(MouseEvent.MOUSE_MOVE, inspectMouseMoveHandler);
					overlayCanvas.addEventListener(MouseEvent.MOUSE_OUT, inspectMouseOutHandler);
					overlayCanvas.addEventListener(MouseEvent.CLICK, inspectMouseClickHandler);
				}
				else if ( mode == ZOOM ) {
					overlayCanvas.addEventListener(MouseEvent.MOUSE_DOWN, zoomMouseDownHandler);
					addEventListener(KeyboardEvent.KEY_UP, zoomKeyUpHandler);
					dispatchEvent(new UpdateStatusBarEvent("click to zoom in, shift-click to zoom out, a to autozoom")); 
				} else if ( mode == PAN ) {
					overlayCanvas.addEventListener(MouseEvent.MOUSE_DOWN, dragMouseDownHandler);
				}
			}
			lastMode = mode;
		}

		private function adjustRectangle(rect: Rectangle): Rectangle {
			var r:Rectangle = rect.clone();
						
			// width and height
			if ( r.width > maximalWidth ) { r.width = maximalWidth }
			if ( r.height > maximalHeight ) { r.height = maximalHeight }
			if ( r.width < MINIMAL_WIDTH ) { r.width = MINIMAL_WIDTH }
			if ( r.height < MINIMAL_WIDTH ) { r.height = MINIMAL_HEIGHT }
			
			// position
			if ( r.x < 0 ) { r.x = 0 }
			if ( r.y < 0 ) { r.y = 0 }
			if ( r.bottomRight.x > maximalWidth ) { r.x -= (r.bottomRight.x - maximalWidth) }
			if ( r.bottomRight.y > maximalHeight ) { r.y -= (r.bottomRight.y - maximalHeight) }

			return r;
		}

		// inspect events
		private function inspectMouseMoveHandler(event:MouseEvent):void {
			var gene:int = currentRectangle.x + event.localX / canvaswidth * currentRectangle.width;
			var sample:int = currentRectangle.y + event.localY / canvasheight * currentRectangle.height;
			dispatchEvent(new BroadcastPositionEvent(BroadcastPositionEvent.MOUSE_OVER, [gene, sample]));			
		}
		private function inspectMouseOutHandler(event:MouseEvent):void {
			dispatchEvent(new BroadcastPositionEvent(BroadcastPositionEvent.MOUSE_OVER, [-1, -1]));			
		}
		private function inspectMouseClickHandler(event:MouseEvent):void {
			var gene:int = currentRectangle.x + event.localX / canvaswidth * currentRectangle.width;
			var sample:int = currentRectangle.y + event.localY / canvasheight * currentRectangle.height;
			dispatchEvent(new BroadcastPositionEvent(BroadcastPositionEvent.MOUSE_CLICK, [gene, sample]));
		}

		// zoom events
		private function zoomMouseDownHandler(event:MouseEvent): void {
			lastClick = getTimer();
			overlayCanvas.addEventListener(MouseEvent.MOUSE_MOVE, zoomMouseMoveHandler);
			overlayCanvas.addEventListener(MouseEvent.MOUSE_UP, zoomMouseUpHandler);
			selectionRectangle = new Rectangle(event.localX, event.localY);
			selection = new Shape();
			overlayCanvas.rawChildren.addChild(selection);
		}
		private function zoomMouseMoveHandler(event:MouseEvent): void {
			overlayCanvas.rawChildren.removeChild(selection);
			selection = new Shape();
			selection.alpha = 0.3;
			selectionRectangle.bottomRight = new Point(event.localX, event.localY);
			selection.graphics.beginFill(0x0000ff);
			selection.graphics.drawRect(selectionRectangle.x, selectionRectangle.y, selectionRectangle.width, selectionRectangle.height); 
			selection.graphics.endFill();
			overlayCanvas.rawChildren.addChild(selection);
		}
		private function zoomMouseUpHandler(event:MouseEvent): void {
			overlayCanvas.removeEventListener(MouseEvent.MOUSE_MOVE, zoomMouseMoveHandler);
			overlayCanvas.removeEventListener(MouseEvent.MOUSE_UP, zoomMouseUpHandler);
			overlayCanvas.removeEventListener(MouseEvent.MOUSE_DOWN, zoomMouseDownHandler);
			if ( getTimer() - lastClick > 200 ) {
				overlayCanvas.rawChildren.removeChild(selection);
	
				var x:Number = currentRectangle.x + selectionRectangle.x / canvaswidth * currentRectangle.width;
				var y:Number = currentRectangle.y + selectionRectangle.y / canvasheight * currentRectangle.height;
				var width:Number = selectionRectangle.width * currentRectangle.width / canvaswidth;
				var height:Number = selectionRectangle.height * currentRectangle.height / canvasheight;
				
				if ( width < 0 ) {
					width *= -1;
					x -= width;
				}
				if ( height < 0 ) {
					height *= -1;
					y -= height;
				}
				
				if ( width != 0 && height != 0 ) { 				
					currentRectangle = new Rectangle(int(x), int(y), int(width)+1, int(height)+1);
					currentRectangle = adjustRectangle(currentRectangle);
				
					drawImage();
					lastRectangle = currentRectangle.clone();
				}
			} else {
				var zoomfactor:Number = 0.5;
				if ( event.shiftKey ) {
					zoomfactor = 1. / zoomfactor;
				}
				var localPt:Point = new Point(event.localX, event.localY);
				currentRectangle = lastRectangle.clone();
	
				var newWidth:int = int(currentRectangle.width * zoomfactor);
				var newHeight:int = int(currentRectangle.height * zoomfactor);
				currentRectangle.width = newWidth;
				currentRectangle.height = newHeight;
				var offsetx:int = int( localPt.x / canvaswidth * lastRectangle.width - newWidth / 2 );
				var offsety:int = int( localPt.y / canvasheight * lastRectangle.height - newHeight / 2 );
				currentRectangle.offset(offsetx, offsety);
				currentRectangle = adjustRectangle(currentRectangle);

				drawImage();
				lastRectangle = currentRectangle.clone();		
			}
			overlayCanvas.addEventListener(MouseEvent.MOUSE_DOWN, zoomMouseDownHandler);
		}
		private function zoomKeyUpHandler(event:KeyboardEvent): void {
			trace(event.keyCode);
			if ( event.keyCode == 0 ) {
				
			} 
		}

		// drag events
		private function dragMouseDownHandler(event:MouseEvent): void {
			var pt:Point = new Point(event.localX, event.localY);
			lastPt = pt;
			overlayCanvas.addEventListener(MouseEvent.MOUSE_MOVE, dragMouseMoveHandler);
			overlayCanvas.addEventListener(MouseEvent.MOUSE_UP, dragMouseUpHandler);
			overlayCanvas.removeEventListener(MouseEvent.MOUSE_DOWN, dragMouseDownHandler);
			currentRectangle = lastRectangle.clone();
		}
		private function dragMouseMoveHandler(event:MouseEvent): void {
			var pt:Point = new Point(event.localX, event.localY);
			currentRectangle = lastRectangle.clone();
			var offsetx:int = int( (lastPt.x - pt.x) / canvaswidth * currentRectangle.width );
			var offsety:int = int( (lastPt.y - pt.y) / canvasheight * currentRectangle.height );
			currentRectangle.offset(offsetx, offsety);
			
			currentRectangle = adjustRectangle(currentRectangle);
			drawImage();
			hscrollbar.scrollPosition = currentRectangle.x;
			vscrollbar.scrollPosition = currentRectangle.y;
		}
		private function dragMouseUpHandler(event:MouseEvent): void {
			var pt:Point = new Point(event.localX, event.localY);
			currentRectangle = lastRectangle.clone();
			var offsetx:int = int( (lastPt.x - pt.x) / canvaswidth * currentRectangle.width );
			var offsety:int = int( (lastPt.y - pt.y) / canvasheight * currentRectangle.height );
			currentRectangle.offset(offsetx, offsety);
			currentRectangle = adjustRectangle(currentRectangle);
			drawImage();
			lastRectangle = currentRectangle.clone();

			hscrollbar.scrollPosition = currentRectangle.x;
			vscrollbar.scrollPosition = currentRectangle.y;
			overlayCanvas.removeEventListener(MouseEvent.MOUSE_MOVE, dragMouseMoveHandler);
			overlayCanvas.removeEventListener(MouseEvent.MOUSE_UP, dragMouseUpHandler);
			overlayCanvas.addEventListener(MouseEvent.MOUSE_DOWN, dragMouseDownHandler);						
		}
		private function hScrollHandler(event:ScrollEvent): void {
			var dx:Number = event.currentTarget.scrollPosition - currentRectangle.x;
			currentRectangle.offset(dx, 0);
			currentRectangle = adjustRectangle(currentRectangle);
			drawImage();
			lastRectangle = currentRectangle.clone();
		}
		
		private function vScrollHandler(event:ScrollEvent): void {
			var dy:Number = event.currentTarget.scrollPosition - currentRectangle.y; 
			currentRectangle.offset(0, dy);
			currentRectangle = adjustRectangle(currentRectangle);
			drawImage();
			lastRectangle = currentRectangle.clone();
		}


		private function drawRectangles(): void {
			if ( canvaswidth > 0 ) {
				modulesCanvas.graphics.clear();
				var r:Rectangle;
				var x:Number; var y:Number; var dx:Number; var dy:Number;
				for ( var module:int = 1; module < modulesOutlines.length; ++module ) {
					r = currentRectangle.intersection(modulesOutlines[module]);
					if ( r.width > 0 && r.height > 0 ) {
						var scalex:Number = canvaswidth / currentRectangle.width;
						var scaley:Number = canvasheight / currentRectangle.height;
						x = (r.x - currentRectangle.x) * scalex;
						y = (r.y - currentRectangle.y) * scaley;
						dx = r.width * scalex;
						dy = r.height * scaley;
						modulesCanvas.graphics.lineStyle(2, modulesColors[module][1]);
						modulesCanvas.graphics.drawRect(x, y, dx, dy);
					}
				}
			}
		}

		private function drawImage(): void {
			drawRectangles();
			var targetRect:Rectangle = new Rectangle(0, 0, canvaswidth, canvasheight);
			currentgeimage.bitmapData = fullgeimage.getData(currentRectangle, targetRect);
			currentmodulesimage.bitmapData = fullmodulesimage.getData(currentRectangle, targetRect);
			dispatchEvent(new HighlightingEvent(HighlightingEvent.MODULE, [highlightedModules]));
			dispatchEvent(new HighlightingEvent(HighlightingEvent.GENE, [highlightedGenes]));
			dispatchEvent(new HighlightingEvent(HighlightingEvent.SAMPLE, [highlightedSamples]));
			updateScrollBars();
 		}
 		
		override protected function createChildren(): void {
			
			super.createChildren();
			
			if ( !modulesimage ) {
				modulesimage = new Image();
				modulesimage.maintainAspectRatio = false;
				modulesimage.source = currentmodulesimage;
				addChild(modulesimage);
			}

			if ( !geimage ) {
				geimage = new Image();
				geimage.maintainAspectRatio = false;
				geimage.source = currentgeimage;
				geimage.alpha = 0.2;
				addChild(geimage);
			}

			if ( !modulesCanvas ) {
				modulesCanvas = new Canvas();
				addChild(modulesCanvas);
			}
				
			if ( !highlightCanvas ) {
				highlightCanvas = new Canvas();
				highlightCanvas.alpha = 1;
				addChild(highlightCanvas);
			}

			if ( !hscrollbar ) {
				hscrollbar = new HScrollBar();
				hscrollbar.addEventListener(ScrollEvent.SCROLL, hScrollHandler);
				addChild(hscrollbar);
			}

			if ( !vscrollbar ) {
				vscrollbar = new VScrollBar();
				vscrollbar.addEventListener(ScrollEvent.SCROLL, vScrollHandler);
				addChild(vscrollbar);
			}

			if ( !overlayCanvas ) {
				overlayCanvas = new Canvas();
				overlayCanvas.alpha = 1;
				overlayCanvas.addEventListener(MouseEvent.MOUSE_MOVE, inspectMouseMoveHandler);
				overlayCanvas.addEventListener(MouseEvent.MOUSE_OUT, inspectMouseOutHandler);
				overlayCanvas.addEventListener(MouseEvent.CLICK, inspectMouseClickHandler);
				addChild(overlayCanvas);
			}
						
			parentApplication.addEventListener(MenuEvent.ALPHA, alphaSliderChangeHandler);
			parentApplication.addEventListener(MenuEvent.OUTLINE, setOutlineVisibilityHandler);
			parentApplication.addEventListener(MenuEvent.FILLING, setFillingVisibilityHandler);
			parentApplication.addEventListener(HighlightingEvent.MODULE, highlightModulesHandler);
			parentApplication.addEventListener(HighlightingEvent.GENE, highlightGenesHandler);
			parentApplication.addEventListener(HighlightingEvent.SAMPLE, highlightSamplesHandler);			
		}

		private function updateScrollBars():void {
			hscrollbar.minScrollPosition = 0
			hscrollbar.maxScrollPosition = maximalWidth - currentRectangle.width;
			hscrollbar.pageSize = currentRectangle.width;
			hscrollbar.lineScrollSize = currentRectangle.width / 4; 
        	hscrollbar.pageScrollSize =  currentRectangle.width * 3 / 4;
			hscrollbar.scrollPosition = currentRectangle.x; 

			vscrollbar.minScrollPosition = 0
			vscrollbar.maxScrollPosition = maximalHeight - currentRectangle.height;
			vscrollbar.pageSize = currentRectangle.height;
			vscrollbar.lineScrollSize = currentRectangle.height / 4; 
        	vscrollbar.pageScrollSize =  currentRectangle.height * 3 / 4;
			vscrollbar.scrollPosition =  currentRectangle.y;			
		}

		override protected function updateDisplayList(unscaledWidth:Number, unscaledHeight:Number):void {
			
			super.updateDisplayList(unscaledWidth, unscaledHeight);
			
			var thickness:Number = 16;
			canvaswidth = unscaledWidth - thickness;
			canvasheight = unscaledHeight - thickness;

			geimage.width = canvaswidth;
			geimage.height = canvasheight;
			modulesimage.width = canvaswidth;
			modulesimage.height = canvasheight;
			modulesCanvas.width = canvaswidth;
			modulesCanvas.height = canvasheight;
			overlayCanvas.width = canvaswidth;
			overlayCanvas.height = canvasheight;
						
			hscrollbar.width = canvaswidth;
			hscrollbar.height = thickness;
			hscrollbar.x = 0;
			hscrollbar.y = canvasheight;
			
			vscrollbar.height = canvasheight;
			vscrollbar.width = thickness;
			vscrollbar.x = canvaswidth;
			vscrollbar.y = 0;
			
			updateScrollBars();
			drawImage();
		}
		
		public function set dataProvider(data: Array): void {
			fullgeimage = data[0];
			fullmodulesimage = data[1];
			modulesOutlines = data[2];
			modulesColors = data[3];
			maximalWidth = fullgeimage.width;
			maximalHeight = fullgeimage.height;
			currentRectangle = new Rectangle(0, 0, maximalWidth, maximalHeight);
			lastRectangle = currentRectangle.clone();
			drawImage();
		}
		
		private function alphaSliderChangeHandler(event:MenuEvent): void {
			geimage.alpha = event.data[0]
		}

		private function setOutlineVisibilityHandler(event:MenuEvent): void {
			modulesCanvas.visible = event.data[0];
		}

		private function setFillingVisibilityHandler(event:MenuEvent): void {
			modulesimage.visible = event.data[0];
		}

		private function highlightModulesHandler(event:HighlightingEvent): void {
			try {
				highlightCanvas.rawChildren.removeChild(highlightCanvas.rawChildren.getChildByName("modules"));
			} catch (err:Error) { 
			}
			
			highlightedModules = event.data[0];
			var shape:Shape = new Shape();			
			shape.name = "modules";
			
			for ( var module:int = 1; module < highlightedModules.length; ++module ) {
				if ( highlightedModules[module] == null ) { 
					continue;
				}
				for ( var i:int = 0; i < highlightedModules[module].length; ++i ) {
					var r:Rectangle = currentRectangle.intersection(highlightedModules[module][i]);
					if ( r.width > 0 && r.height > 0 ) {
						var scalex:Number = canvaswidth / currentRectangle.width;
						var scaley:Number = canvasheight / currentRectangle.height;
						var x:Number = (r.x - currentRectangle.x) * scalex;
						var y:Number = (r.y - currentRectangle.y) * scaley;
						var dx:Number = r.width * scalex;
						var dy:Number = r.height * scaley;
						shape.graphics.beginFill(modulesColors[module][1]);
						shape.graphics.drawRect(x, y, dx, dy);
						shape.graphics.endFill();
					}
				}
			}
			highlightCanvas.rawChildren.addChild(shape);
		}

		private function highlightGenesHandler(event:HighlightingEvent): void {
			try {
				highlightCanvas.rawChildren.removeChild(highlightCanvas.rawChildren.getChildByName("genes"));
			} catch (err:Error) { 
			}
			
			highlightedGenes = event.data[0];
			var shape:Shape = new Shape();
			shape.name = "genes";
			shape.alpha = 0.3;
						
			for ( var i:int = 0; i < highlightedGenes.length; ++i ) {
				var r:Rectangle = currentRectangle.intersection(highlightedGenes[i]);
				if ( r.width > 0 && r.height > 0 ) {
					var scalex:Number = canvaswidth / currentRectangle.width;
					var scaley:Number = canvasheight / currentRectangle.height;
					var x:Number = (r.x - currentRectangle.x) * scalex;
					var y:Number = (r.y - currentRectangle.y) * scaley;
					var dx:Number = r.width * scalex;
					var dy:Number = r.height * scaley;
					shape.graphics.beginFill(0xffff00);
					shape.graphics.drawRect(x, y, dx, dy);
					shape.graphics.endFill();
				}
			}
			highlightCanvas.rawChildren.addChild(shape);
		}
		
		private function highlightSamplesHandler(event:HighlightingEvent): void {
			try {
				highlightCanvas.rawChildren.removeChild(highlightCanvas.rawChildren.getChildByName("samples"));
			} catch (err:Error) { 
			}
			
			highlightedSamples = event.data[0];
			var shape:Shape = new Shape();
			shape.name = "samples";
			shape.alpha = 0.3;
			
			for ( var i:int = 0; i < highlightedSamples.length; ++i ) {
				var r:Rectangle = currentRectangle.intersection(highlightedSamples[i]);
				if ( r.width > 0 && r.height > 0 ) {
					var scalex:Number = canvaswidth / currentRectangle.width;
					var scaley:Number = canvasheight / currentRectangle.height;
					var x:Number = (r.x - currentRectangle.x) * scalex;
					var y:Number = (r.y - currentRectangle.y) * scaley;
					var dx:Number = r.width * scalex;
					var dy:Number = r.height * scaley;
					shape.graphics.beginFill(0xffff00);
					shape.graphics.drawRect(x, y, dx, dy);
					shape.graphics.endFill();
				}
			}
			highlightCanvas.rawChildren.addChild(shape);
		}

		public function addListener(): void {
			parentApplication.addEventListener(MenuEvent.MODE, modeChangeHandler);
		}

		public function removeListener(): void {
			parentApplication.removeEventListener(MenuEvent.MODE, modeChangeHandler);
		}


		public function getBitmap():Bitmap {
			var trans:ColorTransform = new ColorTransform();
			trans.alphaMultiplier = 1 - geimage.alpha;
			var bitmapData:BitmapData = new BitmapData(currentgeimage.width, currentgeimage.height);
			bitmapData.draw(currentgeimage);
			if ( modulesimage.visible ) {
				bitmapData.draw(currentmodulesimage, null, trans);
			}
			if ( modulesCanvas.visible ) {
				bitmapData.draw(modulesCanvas);
			}			
			return new Bitmap(bitmapData);
		}
		
	}
}	

