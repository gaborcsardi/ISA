package ch.unil.cbg.ExpressionView.view.components {
	
	
	import ch.unil.cbg.ExpressionView.events.*;
	import ch.unil.cbg.ExpressionView.model.*;
	
	import com.formatlos.as3.lib.display.BitmapDataUnlimited;
	import com.formatlos.as3.lib.display.events.BitmapDataUnlimitedEvent;
	
	import flash.display.Bitmap;
	import flash.display.BitmapData;
	import flash.display.Shape;
	import flash.events.MouseEvent;
	import flash.geom.Point;
	import flash.geom.Rectangle;
	import flash.utils.getTimer;
	
	import mx.containers.Canvas;
	import mx.controls.Image;

	public class ZoomPanCanvas extends Canvas {
						
		private const MINIMAL_WIDTH:int = 2;
		private const MINIMAL_HEIGHT:int = 2;
	
		private static const INSPECT:int = 0;
    	private static const ZOOM:int = 1;
    	private static const PAN:int = 2;
	
		private var fullgeimage:Bitmap;
		private var fullmodulesimage:Bitmap;
		private var maximalWidth:int = 0;
		private var maximalHeight:int = 0;
		
		public var currentgeimage:Bitmap;
		public var currentmodulesimage:Bitmap;		
		
		private var unlimitedbitmapdata:BitmapDataUnlimited;
		
		private var lastMode:int;
		
		private var lastPt:Point;
		private var currentRectangle:Rectangle;
		private var lastRectangle:Rectangle;
		private var selectionRectangle:Rectangle;
		
		private var selectedGene:int;
		private var selectedSample:int;
		
		private var modulesOutlines:Array;
		private var modulesColors:Vector.<Array>;
		
		private var lastClick:int;
		
		// layout
		private var outerCanvas:Canvas;
		private var geimage:Image;
		private var modulesimage:Image;
		private var modulesCanvas:Canvas;
		private var overlayCanvas:Canvas;
		
		public function ZoomPanCanvas() {
			super();
			lastRectangle = new Rectangle();
			lastPt = new Point();
			
			fullgeimage = new Bitmap();
			currentgeimage = new Bitmap();

			fullmodulesimage = new Bitmap();
			currentmodulesimage = new Bitmap();
						
			lastMode = 0;
			
			unlimitedbitmapdata = new BitmapDataUnlimited();
			unlimitedbitmapdata.addEventListener(BitmapDataUnlimitedEvent.COMPLETE, redrawImage);

		}
		
		private function modeChangeHandler(event:ModeChangeEvent):void {
			var mode:int = event.mode;
			if ( mode != lastMode ) {
				if ( lastMode == INSPECT ) {
					overlayCanvas.removeEventListener(MouseEvent.MOUSE_MOVE, inspectMouseMoveHandler);
				} else if ( lastMode == ZOOM ) {
					overlayCanvas.removeEventListener(MouseEvent.CLICK, zoomClickHandler);
					overlayCanvas.removeEventListener(MouseEvent.MOUSE_DOWN, zoomMouseDownHandler);
					dispatchEvent(new UpdateStatusBarEvent("")); 
				} else if ( lastMode == PAN ) {
					overlayCanvas.removeEventListener(MouseEvent.MOUSE_DOWN, dragMouseDownHandler);					
				}
				
				if ( mode == INSPECT ) {
					overlayCanvas.addEventListener(MouseEvent.MOUSE_MOVE, inspectMouseMoveHandler);
				}
				else if ( mode == ZOOM ) {
					overlayCanvas.addEventListener(MouseEvent.CLICK, zoomClickHandler);
					overlayCanvas.addEventListener(MouseEvent.MOUSE_DOWN, zoomMouseDownHandler);
					dispatchEvent(new UpdateStatusBarEvent("click to zoom in, alt-click to zoom out")); 
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
			var gene:int = lastRectangle.x + event.localX / outerCanvas.width * lastRectangle.width;
			var sample:int = lastRectangle.y + event.localY / outerCanvas.height * lastRectangle.height;
			dispatchEvent(new BroadcastInspectPositionEvent(gene, sample));			
		}

		// zoom events
		private function zoomClickHandler(event:MouseEvent):void {
			if ( getTimer() - lastClick <= 250 ) {
				var zoomfactor:Number = 0.5;
				if ( event.altKey ) {
					zoomfactor = 1. / zoomfactor;
				}
				var localPt:Point = new Point(event.localX, event.localY);
				currentRectangle = lastRectangle.clone();
	
				var newWidth:int = int(currentRectangle.width * zoomfactor);
				var newHeight:int = int(currentRectangle.height * zoomfactor);
				currentRectangle.width = newWidth;
				currentRectangle.height = newHeight;
				var offsetx:int = int( localPt.x / outerCanvas.width * lastRectangle.width - newWidth / 2 );
				var offsety:int = int( localPt.y / outerCanvas.height * lastRectangle.height - newHeight / 2 );
				currentRectangle.offset(offsetx, offsety);
	
				currentRectangle = adjustRectangle(currentRectangle);
				if ( currentRectangle != lastRectangle ) {
					drawImage();
					lastRectangle = currentRectangle;		
				}
			}
			overlayCanvas.removeEventListener(MouseEvent.MOUSE_MOVE, zoomMouseMoveHandler);
		}		
		private function zoomMouseDownHandler(event:MouseEvent): void {
			lastClick = getTimer();
			overlayCanvas.addEventListener(MouseEvent.MOUSE_MOVE, zoomMouseMoveHandler);
			var selection:Shape = new Shape();
			selectionRectangle = new Rectangle(event.localX, event.localY);	
			selection.graphics.drawRect(selectionRectangle.x, selectionRectangle.y, 0, 0);
			overlayCanvas.rawChildren.addChildAt(selection, overlayCanvas.rawChildren.numChildren);
		}
		private function zoomMouseMoveHandler(event:MouseEvent): void {
			overlayCanvas.addEventListener(MouseEvent.MOUSE_UP, zoomMouseUpHandler);
			overlayCanvas.removeEventListener(MouseEvent.MOUSE_DOWN, zoomMouseDownHandler);
			overlayCanvas.rawChildren.removeChildAt(overlayCanvas.rawChildren.numChildren-1);
			var selection:Shape = new Shape();
			selectionRectangle.bottomRight = new Point(event.localX, event.localY);
			selection.graphics.beginFill(0x0000ff);
			selection.graphics.drawRect(selectionRectangle.x, selectionRectangle.y, selectionRectangle.width, selectionRectangle.height); 
			selection.graphics.endFill();
			overlayCanvas.rawChildren.addChildAt(selection, overlayCanvas.rawChildren.numChildren);
		}
		private function zoomMouseUpHandler(event:MouseEvent): void {
			if ( getTimer() - lastClick > 250 ) {
				overlayCanvas.rawChildren.removeChildAt(overlayCanvas.rawChildren.numChildren-1);
				overlayCanvas.removeEventListener(MouseEvent.MOUSE_MOVE, zoomMouseMoveHandler);
				overlayCanvas.removeEventListener(MouseEvent.MOUSE_UP, zoomMouseUpHandler);	
	
				var x:Number = currentRectangle.x + selectionRectangle.x / outerCanvas.width * currentRectangle.width;
				var y:Number = currentRectangle.y + selectionRectangle.y / outerCanvas.height * currentRectangle.height;
				var width:Number = selectionRectangle.width * currentRectangle.width / outerCanvas.width;
				var height:Number = selectionRectangle.height * currentRectangle.height / outerCanvas.height;
				currentRectangle = new Rectangle(int(x), int(y), int(width)+1, int(height)+1);
				currentRectangle = adjustRectangle(currentRectangle);
				
				if ( currentRectangle != lastRectangle ) {
					drawImage();
					lastRectangle = currentRectangle.clone();
				}
			}
			overlayCanvas.addEventListener(MouseEvent.MOUSE_DOWN, zoomMouseDownHandler);
			
		}

		// drag events
		private function dragMouseDownHandler(event:MouseEvent): void {
			var pt:Point = new Point(event.localX, event.localY);
			lastPt = pt;
			overlayCanvas.addEventListener(MouseEvent.MOUSE_MOVE, dragMouseMoveHandler);
			overlayCanvas.addEventListener(MouseEvent.MOUSE_UP, dragMouseUpHandler);
			overlayCanvas.removeEventListener(MouseEvent.MOUSE_DOWN, dragMouseDownHandler);
		}
		private function dragMouseMoveHandler(event:MouseEvent): void {
			var pt:Point = new Point(event.localX, event.localY);
			currentRectangle = lastRectangle.clone();
			var offsetx:int = int( (lastPt.x - pt.x) / outerCanvas.width * currentRectangle.width );
			var offsety:int = int( (lastPt.y - pt.y) / outerCanvas.height * currentRectangle.height );
			currentRectangle.offset(offsetx, offsety);
			
			currentRectangle = adjustRectangle(currentRectangle);
			if ( currentRectangle != lastRectangle ) {
				drawImage();
			}
		}
		private function dragMouseUpHandler(event:MouseEvent): void {
			var pt:Point = new Point(event.localX, event.localY);
			currentRectangle = lastRectangle.clone();
			var offsetx:int = int( (lastPt.x - pt.x) / outerCanvas.width * currentRectangle.width );
			var offsety:int = int( (lastPt.y - pt.y) / outerCanvas.height * currentRectangle.height );
			currentRectangle.offset(offsetx, offsety);
			currentRectangle = adjustRectangle(currentRectangle);
			if ( currentRectangle != lastRectangle ) {
				drawImage();
				lastRectangle = currentRectangle.clone();
			}

			overlayCanvas.removeEventListener(MouseEvent.MOUSE_MOVE, dragMouseMoveHandler);
			overlayCanvas.removeEventListener(MouseEvent.MOUSE_UP, dragMouseUpHandler);
			overlayCanvas.addEventListener(MouseEvent.MOUSE_DOWN, dragMouseDownHandler);						
		}
		
		private function drawRectangles(): void {
			modulesCanvas.graphics.clear();
			var r:Rectangle;
			var x:Number; var y:Number; var dx:Number; var dy:Number;
			for ( var module:int = 0; module < modulesOutlines.length; ++module ) {
				r = currentRectangle.intersection(modulesOutlines[module]);
				if ( r.width > 0 && r.height > 0 ) {
					var scalex:Number = modulesCanvas.width / currentRectangle.width;
					var scaley:Number = modulesCanvas.height / currentRectangle.height;
					x = (r.x - currentRectangle.x) * scalex;
					y = (r.y - currentRectangle.y) * scaley;
					dx = r.width * scalex;
					dy = r.height * scaley;
					modulesCanvas.graphics.lineStyle(2, modulesColors[module][1], 1);
					modulesCanvas.graphics.drawRect(x, y, dx, dy);
				}
			}
		}
				
		private function drawImage(): void {
						
			if ( currentRectangle.width == lastRectangle.width && currentRectangle.height == lastRectangle.height ) {
				
				drawRectangles();
						
				var bitmapdata:BitmapData = unlimitedbitmapdata.bitmapData.clone();			

				bitmapdata.copyPixels(fullgeimage.bitmapData, currentRectangle, new Point(0, 0));
				currentgeimage.bitmapData = bitmapdata.clone();
				currentgeimage.width = outerCanvas.width;
				currentgeimage.height = outerCanvas.height;

				bitmapdata.copyPixels(fullmodulesimage.bitmapData, currentRectangle, new Point(0, 0));
				currentmodulesimage.bitmapData = bitmapdata.clone();
				currentmodulesimage.width = outerCanvas.width;
				currentmodulesimage.height = outerCanvas.height;

				bitmapdata.dispose();
				
			} else {
 				unlimitedbitmapdata.create(currentRectangle.width, currentRectangle.height, false);
 			}
		}

		private function redrawImage(event:BitmapDataUnlimitedEvent): void {

			drawRectangles();
			
			var bitmapdata:BitmapData = unlimitedbitmapdata.bitmapData.clone();			
			
			bitmapdata.copyPixels(fullgeimage.bitmapData, currentRectangle, new Point(0, 0));
			currentgeimage.bitmapData = bitmapdata.clone();
			currentgeimage.width = outerCanvas.width;
			currentgeimage.height = outerCanvas.height;
			
			bitmapdata.copyPixels(fullmodulesimage.bitmapData, currentRectangle, new Point(0, 0));
			currentmodulesimage.bitmapData = bitmapdata.clone();
			currentmodulesimage.width = outerCanvas.width;
			currentmodulesimage.height = outerCanvas.height;
			
			bitmapdata.dispose();
			invalidateDisplayList();						
		}

			
		override protected function createChildren(): void {
			
			super.createChildren();
			
			if ( !outerCanvas ) {
				outerCanvas = new Canvas();
				addChild(outerCanvas);
					
				if ( !modulesimage ) {
					modulesimage = new Image();
					modulesimage.maintainAspectRatio = false;
					modulesimage.source = currentmodulesimage;
					modulesimage.alpha = 1;
					modulesimage.visible = true;
					outerCanvas.addChild(modulesimage);
				}

				if ( !geimage ) {
					geimage = new Image();
					geimage.maintainAspectRatio = false;
					geimage.source = currentgeimage;
					geimage.alpha = 0.1;
					geimage.visible = true;
					outerCanvas.addChild(geimage);
				}

				if ( !modulesCanvas ) {
					modulesCanvas = new Canvas();
					outerCanvas.addChild(modulesCanvas);
				}
					
				if ( !overlayCanvas ) {
					overlayCanvas = new Canvas();
					overlayCanvas.alpha = 0.3;
					overlayCanvas.addEventListener(MouseEvent.MOUSE_MOVE, inspectMouseMoveHandler);
					outerCanvas.addChild(overlayCanvas);
				}
				
			}
			
			parentApplication.addEventListener(AlphaSliderChangeEvent.ALPHASLIDERCHANGEEVENT, alphaSliderChangeHandler);
			parentApplication.addEventListener(SetOutlineVisibilityEvent.SETOUTLINEVISIBILITYEVENT, setOutlineVisibilityHandler);
			parentApplication.addEventListener(SetFillingVisibilityEvent.SETFILLINGVISIBILITYEVENT, setFillingVisibilityHandler);
			parentApplication.addEventListener(UpdateHighlightedModulesEvent.UPDATEHIGHLIGHTEDMODULESEVENT, updateHighlightedModulesHandler);			
			
		}

		override protected function updateDisplayList(unscaledWidth:Number, unscaledHeight:Number):void {
			super.updateDisplayList(unscaledWidth, unscaledHeight);
			
			outerCanvas.percentWidth = 100;
			outerCanvas.percentHeight = 100;
			geimage.percentWidth = 100;
			geimage.percentHeight = 100;
			modulesimage.percentWidth = 100;
			modulesimage.percentHeight = 100;
			modulesCanvas.percentWidth = 100;
			modulesCanvas.percentHeight = 100;
			overlayCanvas.percentWidth = 100;
			overlayCanvas.percentHeight = 100;			
		}
		
		public function set colors(data:Vector.<Array>): void {
			modulesColors = data;
		}
		
		public function set dataProvider(data: Array): void {
			fullgeimage = data[0];
			fullmodulesimage = data[1];
			modulesOutlines = data[2];
			maximalWidth = fullgeimage.width;
			maximalHeight = fullgeimage.height;
			currentRectangle = new Rectangle(0, 0, maximalWidth, maximalHeight);
			lastRectangle = new Rectangle(0, 0, maximalWidth+1, maximalHeight+1);
			drawImage();
		}
		
		private function alphaSliderChangeHandler(event:AlphaSliderChangeEvent): void {
			geimage.alpha = event.alpha;
		}

		private function setOutlineVisibilityHandler(event:SetOutlineVisibilityEvent): void {
			modulesCanvas.visible = event.visible;
		}

		private function setFillingVisibilityHandler(event:SetFillingVisibilityEvent): void {
			modulesimage.visible = event.visible;
		}

		private function updateHighlightedModulesHandler(event:UpdateHighlightedModulesEvent): void {
			overlayCanvas.graphics.clear();
			
			var nRectangles:int = event.modulesRectangles.length;
			if ( nRectangles > 0 ) {
				var i:int;
				var r:Rectangle;
				var x:Number; var y:Number; var dx:Number; var dy:Number;
				for ( i = 0; i < nRectangles; ++i ) {
					r = currentRectangle.intersection(event.modulesRectangles[i]);
					if ( r.width > 0 && r.height > 0 ) {
						var scalex:Number = overlayCanvas.width / currentRectangle.width;
						var scaley:Number = overlayCanvas.height / currentRectangle.height;
						x = (r.x - currentRectangle.x) * scalex;
						y = (r.y - currentRectangle.y) * scaley;
						dx = r.width * scalex;
						dy = r.height * scaley;
						overlayCanvas.graphics.beginFill(0xffff00);
						overlayCanvas.graphics.drawRect(x, y, dx, dy);
						overlayCanvas.graphics.endFill();
					}
				}
			}
			
		}

		public function addListener(): void {
			parentApplication.addEventListener(ModeChangeEvent.MODECHANGEEVENT, modeChangeHandler);
		}

		public function removeListener(): void {
			parentApplication.removeEventListener(ModeChangeEvent.MODECHANGEEVENT, modeChangeHandler);
		}

				
	}
}	

