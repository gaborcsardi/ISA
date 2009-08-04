// modified version of the SuperPanel by Wietse Veenstra (http://www.wietseveenstra.nl)
package ch.unil.cbg.ExpressionView.view.components {
	
	import ch.unil.cbg.ExpressionView.events.ResizablePanelEvent;
	
	import flash.events.MouseEvent;
	import flash.geom.Point;
	import flash.geom.Rectangle;
	
	import mx.containers.Panel;
	import mx.controls.Button;
	import mx.core.UIComponent;
	import mx.effects.Resize;
	import mx.events.DragEvent;
	import mx.events.EffectEvent;
	import mx.managers.CursorManager;
	
	[Event(name=ResizablePanelEvent.CLOSE, type="ch.unil.cbg.expressionview.events.ResizablePanelEvent")];
	[Event(name=ResizablePanelEvent.MAXIMIZE, type="ch.unil.cbg.expressionview.events.ResizablePanelEvent")];
	[Event(name=ResizablePanelEvent.MINIMIZE, type="ch.unil.cbg.expressionview.events.ResizablePanelEvent")];
	[Event(name=ResizablePanelEvent.RESIZE, type="ch.unil.cbg.expressionview.events.ResizablePanelEvent")];
	public class ResizablePanel extends Panel {
		
		public var enableResize:Boolean = true;
		public var enableClose:Boolean = true;
		public var enableMaximize:Boolean = true;
		public var enableMinimize:Boolean = true;
				
		[Embed(source="/ch/unil/cbg/ExpressionView/assets/cursor/resizeCursor.png")]
		private static var resizeCursor:Class;
		
		private var	resizableTitleBar:UIComponent;
		
		private const MIN_WIDTH:Number = 100;
		private const MIN_HEIGHT:Number = 100;
		
		private var oldWidth:Number;
		private var oldHeight:Number;
		private var oldX:Number;
		private var oldY:Number;
		
		private var resizeButton:Button;
		private var closeButton:Button;
		private var maximizeButton:Button;
		private var minimizeButton:Button;
		
		private var upMotion:Resize	= new Resize();
		private var downMotion:Resize = new Resize();
		
		private var minimized:Boolean = false;
		
		private var oPoint:Point = new Point();
		
		private var resizeCur:Number = 0;
				
		public function ResizablePanel() {
			super();
		}

		override protected function createChildren():void {
			super.createChildren();
			styleName = "resizablePanel";
			doubleClickEnabled = true;
			
			if ( !resizableTitleBar ) {
				resizableTitleBar = super.titleBar;
			}
			
			if ( enableResize ) {
				if ( !resizeButton ) {
					resizeButton = new Button();
					resizeButton.width = 12;
					resizeButton.height = 12;
					resizeButton.styleName = "resizeButton";
					rawChildren.addChild(resizeButton);
				}
			}
			
			if ( enableMinimize ) {
				if ( !minimizeButton ) {
					minimizeButton = new Button();
					minimizeButton.width = 10;
					minimizeButton.height = 10;
					minimizeButton.buttonMode = true;
					minimizeButton.useHandCursor = true;
					minimizeButton.styleName = "minimizeButton";
					resizableTitleBar.addChild(minimizeButton);
				}
			}
			
			if ( enableMaximize ) {	
				if ( !maximizeButton ) {
					maximizeButton = new Button();
					maximizeButton.width = 10;
					maximizeButton.height = 10;
					maximizeButton.buttonMode = true;
					maximizeButton.useHandCursor = true;
					maximizeButton.styleName = "maximizeButtonIncrease";
					resizableTitleBar.addChild(maximizeButton);
				}
			}

			if ( enableClose ) {
				if ( !closeButton ) {
					closeButton = new Button();
					closeButton.width = 10;
					closeButton.height = 10;
					closeButton.buttonMode = true;
					closeButton.useHandCursor = true;
					closeButton.styleName = "closeButton";
					resizableTitleBar.addChild(closeButton);
				}
			}
	
			initPos();
			addListeners();
		}
		
		private function initPos():void {
			oldWidth = width;
			oldHeight = height;
			oldX = x;
			oldY = y;
		}
	
		override protected function updateDisplayList(unscaledWidth:Number, unscaledHeight:Number):void {
			super.updateDisplayList(unscaledWidth, unscaledHeight);
			
			if ( enableResize ) {
				resizeButton.y = unscaledHeight - resizeButton.height - 1;
				resizeButton.x = unscaledWidth - resizeButton.width - 1;
			}
			
			var x:Number = 0;
			if ( enableClose ) {
				x += closeButton.width + 8;
				closeButton.x = unscaledWidth - x;
				closeButton.y = ( resizableTitleBar.height - minimizeButton.height ) / 2;
			}
			if ( enableMaximize ) {
				x += maximizeButton.width + 8;
				maximizeButton.x = unscaledWidth - x;
				maximizeButton.y = ( resizableTitleBar.height - maximizeButton.height ) / 2;
			}
			if ( enableMinimize ) {
				x += minimizeButton.width + 8;
				minimizeButton.x = unscaledWidth - x;
				minimizeButton.y = ( resizableTitleBar.height - maximizeButton.height ) / 2;
			}
			
		}
			
		private function addListeners():void {
			addEventListener(MouseEvent.CLICK, panelClickHandler);
			resizableTitleBar.addEventListener(MouseEvent.MOUSE_DOWN, titleBarDownHandler);
			resizableTitleBar.addEventListener(MouseEvent.DOUBLE_CLICK, titleBarDoubleClickHandler);
			
			if (enableResize) {
				resizeButton.addEventListener(MouseEvent.MOUSE_OVER, resizeOverHandler);
				resizeButton.addEventListener(MouseEvent.MOUSE_OUT, resizeOutHandler);
				resizeButton.addEventListener(MouseEvent.MOUSE_DOWN, resizeDownHandler);
			}

			if ( enableMinimize ) {
			//	minimizeButton.addEventListener(MouseEvent.CLICK, minimizeClickHandler);
			}
			
			if ( enableMaximize ) {
				maximizeButton.addEventListener(MouseEvent.CLICK, maximizeClickHandler);
			}

			if ( enableClose ) {
				closeButton.addEventListener(MouseEvent.CLICK, closeClickHandler);
			}


		}
		
		private function panelClickHandler(event:MouseEvent):void {
			resizableTitleBar.removeEventListener(MouseEvent.MOUSE_MOVE, titleBarMoveHandler);
			panelFocusCheckHandler();
		}
		
		private function titleBarDownHandler(event:MouseEvent):void {
			resizableTitleBar.addEventListener(MouseEvent.MOUSE_MOVE, titleBarMoveHandler);
			panelFocusCheckHandler();
		}

		private function titleBarMoveHandler(event:MouseEvent):void {
			if ( width < parent.width) {
				parent.addEventListener(MouseEvent.MOUSE_UP, titleBarDragDropHandler);
				resizableTitleBar.addEventListener(DragEvent.DRAG_DROP,titleBarDragDropHandler);
				panelFocusCheckHandler();
				alpha = 0.5;
				startDrag(false, new Rectangle(0, 0, parent.width - width, parent.height - height));
			}
		}
		
		private function titleBarDragDropHandler(event:MouseEvent):void {
			resizableTitleBar.removeEventListener(MouseEvent.MOUSE_MOVE, titleBarMoveHandler);
			alpha = 1.0;
			stopDrag();
			initPos();
		}
		
		private function panelFocusCheckHandler():void {
			parent.setChildIndex(this, parent.numChildren - 1);
			/*
			for (var i:int = 0; i < parent.numChildren; i++) {
				var child:UIComponent = UIComponent(parent.getChildAt(i));
				if (parent.getChildIndex(child) < parent.numChildren - 1) {
					child.setStyle("headerColors", [0xC3D1D9, 0xD2DCE2]);
					child.setStyle("borderColor", 0xD2DCE2);
				} else if (parent.getChildIndex(child) == parent.numChildren - 1) {
					child.setStyle("headerColors", [0xC3D1D9, 0x5A788A]);
					child.setStyle("borderColor", 0x5A788A);
				}
			}
			*/
		}

		private function titleBarDoubleClickHandler(event:MouseEvent):void {
			resizableTitleBar.removeEventListener(MouseEvent.MOUSE_MOVE, titleBarMoveHandler);
			parent.removeEventListener(MouseEvent.MOUSE_UP, resizeUpHandler);

			upMotion.target = this;
			upMotion.duration = 300;
			upMotion.heightFrom = height;
			upMotion.heightTo = resizableTitleBar.height;;
			upMotion.end();
			
			downMotion.target = this;
			downMotion.duration = 300;
			downMotion.heightFrom = resizableTitleBar.height;
			downMotion.heightTo = oldHeight;
			downMotion.end();
			
			if ( !minimized ) {
				initPos();
				upMotion.play();
				resizeButton.visible = false;
				minimized = true;
			} else {
				downMotion.play();
				downMotion.addEventListener(EffectEvent.EFFECT_END, endEffectEventHandler);
				minimized = false;
			}
		}
						
		private function endEffectEventHandler(event:EffectEvent):void {
			resizeButton.visible = true;
		}

		private function maximizeClickHandler(event:MouseEvent):void {
			if ( maximizeButton.styleName == "maximizeButtonIncrease") {
				if ( height > resizableTitleBar.height ) {
					initPos();
					x = 0;
					y = 0;
					width = parent.width;
					height = parent.height;
					maximizeButton.styleName = "maximizeButtonDecrease";
				}
			} else {
				x = oldX;
				y = oldY;
				width = oldWidth;
				height = oldHeight;
				maximizeButton.styleName = "maximizeButtonIncrease";
			}
		}
		
		private function closeClickHandler(event:MouseEvent):void {
			removeEventListener(MouseEvent.CLICK, panelClickHandler);
			dispatchEvent(new ResizablePanelEvent(ResizablePanelEvent.CLOSE))
		}
		
		private function resizeOverHandler(event:MouseEvent):void {
			resizeCur = CursorManager.setCursor(resizeCursor);
		}
		
		private function resizeOutHandler(event:MouseEvent):void {
			CursorManager.removeCursor(CursorManager.currentCursorID);
		}
		
		private function resizeDownHandler(event:MouseEvent):void {
			panelFocusCheckHandler();
			parent.addEventListener(MouseEvent.MOUSE_MOVE, resizeMoveHandler);
			parent.addEventListener(MouseEvent.MOUSE_UP, resizeUpHandler);
			resizeButton.addEventListener(MouseEvent.MOUSE_OVER, resizeOverHandler)
			panelClickHandler(event);
			resizeCur = CursorManager.setCursor(resizeCursor);
			oPoint.x = parent.mouseX;
			oPoint.y = parent.mouseY;
			//initPos();	
		}
		
		private function resizeMoveHandler(event:MouseEvent):void {
			stopDragging();

			var xPlus:Number = parent.mouseX - oPoint.x;
			var yPlus:Number = parent.mouseY - oPoint.y;

			if ( oldWidth + xPlus > MIN_WIDTH && x + oldWidth + xPlus <= parent.width ) {
				width = oldWidth + xPlus;
			}
			
			if ( oldHeight + yPlus > MIN_HEIGHT && y + oldHeight + yPlus <= parent.height ) {
				height = oldHeight + yPlus;
			}
		}
		
		private function resizeUpHandler(event:MouseEvent):void {
			parent.removeEventListener(MouseEvent.MOUSE_MOVE, resizeMoveHandler);
			parent.removeEventListener(MouseEvent.MOUSE_UP, resizeUpHandler);
			CursorManager.removeCursor(CursorManager.currentCursorID);
			resizeButton.addEventListener(MouseEvent.MOUSE_OVER, resizeOverHandler);
			initPos();
		}
		
	}
	
}