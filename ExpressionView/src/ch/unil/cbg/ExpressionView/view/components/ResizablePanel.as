// modified version of the SuperPanel by Wietse Veenstra (http://www.wietseveenstra.nl)
package ch.unil.cbg.ExpressionView.view.components {
	
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
	
	public class ResizablePanel extends Panel {
		[Bindable] public var showControls:Boolean = true;
		[Bindable] public var enableResize:Boolean = true;
				
		[Embed(source="/ch/unil/cbg/ExpressionView/assets/cursor/resizeCursor.png")]
		private static var resizeCursor:Class;
		
		private var	pTitleBar:UIComponent;
		private var oW:Number;
		private var oH:Number;
		private var oX:Number;
		private var oY:Number;
		private var normalMaxButton:Button;
		private var closeButton:Button;
		private var resizeHandler:Button;
		private var upMotion:Resize			= new Resize();
		private var downMotion:Resize		= new Resize();
		private var oPoint:Point 			= new Point();
		private var resizeCur:Number		= 0;
				
		public function ResizablePanel() {
			super();
		}

		override protected function createChildren():void {
			super.createChildren();
			styleName = "resizablePanel";
			doubleClickEnabled = true;
			
			if ( !pTitleBar ) {
				pTitleBar = super.titleBar;
			}
			
			if ( enableResize ) {
				if ( !resizeHandler ) {
					resizeHandler = new Button();
					resizeHandler.width     = 12;
					resizeHandler.height    = 12;
					resizeHandler.styleName = "resizeHndlr";
					this.rawChildren.addChild(resizeHandler);
				}
				initPos();
			}
			
			if (showControls) {
				
				if ( !normalMaxButton ) {
					normalMaxButton = new Button();
					normalMaxButton.width     	= 10;
					normalMaxButton.height    	= 10;
					normalMaxButton.styleName 	= "increaseBtn";
					pTitleBar.addChild(this.normalMaxButton);
				}
				
				if ( !closeButton ) {
					closeButton = new Button();
					closeButton.width     		= 10;
					closeButton.height    		= 10;
					closeButton.styleName 		= "closeBtn";
					pTitleBar.addChild(this.closeButton);
				}
				
			}
			
			this.positionChildren();	
			this.addListeners();
		}
		
		private function initPos():void {
			this.oW = this.width;
			this.oH = this.height;
			this.oX = this.x;
			this.oY = this.y;
		}
	
		override protected function updateDisplayList(unscaledWidth:Number, unscaledHeight:Number):void {
			super.updateDisplayList(unscaledWidth, unscaledHeight);
			if ( showControls ) {
				normalMaxButton.x = unscaledWidth - normalMaxButton.width - 24;
				normalMaxButton.y = 8;
				closeButton.x = unscaledWidth - closeButton.width - 8;
				closeButton.y = 8;
			}
			if ( enableResize ) {
				resizeHandler.y = unscaledHeight - resizeHandler.height - 1;
				resizeHandler.x = unscaledWidth - resizeHandler.width - 1;
			}
		}
	
		private function positionChildren():void {
			if (showControls) {
				this.normalMaxButton.buttonMode    = true;
				this.normalMaxButton.useHandCursor = true;
				this.normalMaxButton.x = this.unscaledWidth - this.normalMaxButton.width - 24;
				this.normalMaxButton.y = 8;
				this.closeButton.buttonMode	   = true;
				this.closeButton.useHandCursor = true;
				this.closeButton.x = this.unscaledWidth - this.closeButton.width - 8;
				this.closeButton.y = 8;
			}
			
			if (enableResize) {
				this.resizeHandler.y = this.unscaledHeight - resizeHandler.height - 1;
				this.resizeHandler.x = this.unscaledWidth - resizeHandler.width - 1;
			}
		}
		
		private function addListeners():void {
			this.addEventListener(MouseEvent.CLICK, panelClickHandler);
			this.pTitleBar.addEventListener(MouseEvent.MOUSE_DOWN, titleBarDownHandler);
			this.pTitleBar.addEventListener(MouseEvent.DOUBLE_CLICK, titleBarDoubleClickHandler);
			
			if (showControls) {
				this.closeButton.addEventListener(MouseEvent.CLICK, closeClickHandler);
				this.normalMaxButton.addEventListener(MouseEvent.CLICK, normalMaxClickHandler);
			}
			
			if (enableResize) {
				this.resizeHandler.addEventListener(MouseEvent.MOUSE_OVER, resizeOverHandler);
				this.resizeHandler.addEventListener(MouseEvent.MOUSE_OUT, resizeOutHandler);
				this.resizeHandler.addEventListener(MouseEvent.MOUSE_DOWN, resizeDownHandler);
			}
		}
		
		private function panelClickHandler(event:MouseEvent):void {
			this.pTitleBar.removeEventListener(MouseEvent.MOUSE_MOVE, titleBarMoveHandler);
			//this.parent.setChildIndex(this, this.parent.numChildren - 1);
			this.panelFocusCheckHandler();
		}
		
		private function titleBarDownHandler(event:MouseEvent):void {
			this.pTitleBar.addEventListener(MouseEvent.MOUSE_MOVE, titleBarMoveHandler);
		}
			
		private function titleBarMoveHandler(event:MouseEvent):void {
			if (this.width < parent.width) {
				//Application.application.parent.addEventListener(MouseEvent.MOUSE_UP, titleBarDragDropHandler);
				parent.addEventListener(MouseEvent.MOUSE_UP, titleBarDragDropHandler);
				this.pTitleBar.addEventListener(DragEvent.DRAG_DROP,titleBarDragDropHandler);
				//this.parent.setChildIndex(this, this.parent.numChildren - 1);
				this.panelFocusCheckHandler();
				this.alpha = 0.5;
				this.startDrag(false, new Rectangle(0, 0, parent.width - this.width, parent.height - this.height));
			}
		}
		
		private function titleBarDragDropHandler(event:MouseEvent):void {
			this.pTitleBar.removeEventListener(MouseEvent.MOUSE_MOVE, titleBarMoveHandler);
			this.alpha = 1.0;
			this.stopDrag();
		}
		
		private function panelFocusCheckHandler():void {
			for (var i:int = 0; i < this.parent.numChildren; i++) {
				var child:UIComponent = UIComponent(this.parent.getChildAt(i));
				if (this.parent.getChildIndex(child) < this.parent.numChildren - 1) {
					child.setStyle("headerColors", [0xC3D1D9, 0xD2DCE2]);
					child.setStyle("borderColor", 0xD2DCE2);
				} else if (this.parent.getChildIndex(child) == this.parent.numChildren - 1) {
					child.setStyle("headerColors", [0xC3D1D9, 0x5A788A]);
					child.setStyle("borderColor", 0x5A788A);
				}
			}
		}
		
		private function titleBarDoubleClickHandler(event:MouseEvent):void {
			this.pTitleBar.removeEventListener(MouseEvent.MOUSE_MOVE, titleBarMoveHandler);
			//Application.application.parent.removeEventListener(MouseEvent.MOUSE_UP, resizeUpHandler);
			parent.removeEventListener(MouseEvent.MOUSE_UP, resizeUpHandler);
			
			this.upMotion.target = this;
			this.upMotion.duration = 300;
			this.upMotion.heightFrom = oH;
			this.upMotion.heightTo = 28;
			this.upMotion.end();
			
			this.downMotion.target = this;
			this.downMotion.duration = 300;
			this.downMotion.heightFrom = 28;
			this.downMotion.heightTo = oH;
			this.downMotion.end();
			
			if (this.width < parent.width) {
				if (this.height == oH) {
					this.upMotion.play();
					this.resizeHandler.visible = false;
				} else {
					this.downMotion.play();
					this.downMotion.addEventListener(EffectEvent.EFFECT_END, endEffectEventHandler);
				}
			}
		}
						
		private function endEffectEventHandler(event:EffectEvent):void {
			this.resizeHandler.visible = true;
		}

		private function normalMaxClickHandler(event:MouseEvent):void {
			if (this.normalMaxButton.styleName == "increaseBtn") {
				if (this.height > 28) {
					this.initPos();
					this.x = 0;
					this.y = 0;
					this.width = parent.width;
					this.height = parent.height;
					this.normalMaxButton.styleName = "decreaseBtn";
					this.positionChildren();
				}
			} else {
				this.x = this.oX;
				this.y = this.oY;
				this.width = this.oW;
				this.height = this.oH;
				this.normalMaxButton.styleName = "increaseBtn";
				this.positionChildren();
			}
		}
		
		private function closeClickHandler(event:MouseEvent):void {
			this.removeEventListener(MouseEvent.CLICK, panelClickHandler);
			//this.parent.removeChild(this);
			this.visible = false; 
		}
		
		private function resizeOverHandler(event:MouseEvent):void {
			this.resizeCur = CursorManager.setCursor(resizeCursor);
		}
		
		private function resizeOutHandler(event:MouseEvent):void {
			CursorManager.removeCursor(CursorManager.currentCursorID);
		}
		
		private function resizeDownHandler(event:MouseEvent):void {
			//Application.application.parent.addEventListener(MouseEvent.MOUSE_MOVE, resizeMoveHandler);
			//Application.application.parent.addEventListener(MouseEvent.MOUSE_UP, resizeUpHandler);
			parent.addEventListener(MouseEvent.MOUSE_MOVE, resizeMoveHandler);
			parent.addEventListener(MouseEvent.MOUSE_UP, resizeUpHandler);
			this.resizeHandler.addEventListener(MouseEvent.MOUSE_OVER, resizeOverHandler);
			this.panelClickHandler(event);
			this.resizeCur = CursorManager.setCursor(resizeCursor);
			this.oPoint.x = mouseX;
			this.oPoint.y = mouseY;
			this.oPoint = this.localToGlobal(oPoint);		
		}
		
		private function resizeMoveHandler(event:MouseEvent):void {
			this.stopDragging();

			//var xPlus:Number = Application.application.parent.mouseX - this.oPoint.x;			
			//var yPlus:Number = Application.application.parent.mouseY - this.oPoint.y;
			var xPlus:Number = parent.mouseX - oPoint.x;
			var yPlus:Number = parent.mouseY - oPoint.y;
			
			if ( oW + xPlus > 140 && this.x + oW + xPlus <= parent.width ) {
				this.width = this.oW + xPlus;
			}
			
			if (this.oH + yPlus > 80 && this.y + oH + yPlus <= parent.height ) {
				this.height = this.oH + yPlus;
			}
			positionChildren();
		}
		
		private function resizeUpHandler(event:MouseEvent):void {
			//Application.application.parent.removeEventListener(MouseEvent.MOUSE_MOVE, resizeMoveHandler);
			//Application.application.parent.removeEventListener(MouseEvent.MOUSE_UP, resizeUpHandler);
			parent.removeEventListener(MouseEvent.MOUSE_MOVE, resizeMoveHandler);
			parent.removeEventListener(MouseEvent.MOUSE_UP, resizeUpHandler);
			CursorManager.removeCursor(CursorManager.currentCursorID);
			this.resizeHandler.addEventListener(MouseEvent.MOUSE_OVER, resizeOverHandler);
			initPos();
		}
	}
	
}