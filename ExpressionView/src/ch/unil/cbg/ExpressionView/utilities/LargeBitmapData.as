package ch.unil.cbg.ExpressionView.utilities {
	
	import __AS3__.vec.Vector;
	
	import flash.display.BitmapData;
	import flash.geom.Matrix;
	import flash.geom.Point;
	import flash.geom.Rectangle;

	public class LargeBitmapData {
	
		private const LIMIT:Number = 8000;
		private const OVERLAP:Number = 1000;
	
		private var bitmaps:Vector.<BitmapData>;
		private var rectangles:Vector.<Rectangle>;
		private var dimx:int;
		private var dimy:int;
		private var dim:int;
		
		public var width:Number;
		public var height:Number;
	
		public function LargeBitmapData(_width:Number = 1, _height:Number = 1) {
			width = _width;
			height = _height;
			dimx = int(width / LIMIT) + 1;
			dimy = int(height / LIMIT) + 1;
			dim = dimx * dimy;

			bitmaps = new Vector.<BitmapData>(dim, false);
			rectangles = new Vector.<Rectangle>(dim, false);
			for ( var x:int = 0; x < dimx; ++x ) {
				for ( var y:int = 0; y < dimy; ++y ) {
					var k:int = y * dimx + x;
					var w:Number = LIMIT;
					var h:Number = LIMIT;
					if ( x == dimx - 1 ) {
						w = width % LIMIT;
					}
					if ( y == dimy - 1 ) {
						h = height % LIMIT;
					}
					bitmaps[k] = new BitmapData(w, h);
					rectangles[k] = new Rectangle(x * LIMIT, y * LIMIT, w, h);
				}
			}
		}
		
		public function setPixel(x:Number, y:Number, value:uint):void {
			var position:Array = map(x, y);
			bitmaps[position[0]].setPixel(position[1], position[2], value);
		}

		public function getPixel(x:Number, y:Number):uint {
			var position:Array = map(x, y);
			return bitmaps[position[0]].getPixel(position[1], position[2]);
		}
		
		
		/*
		public function getData(sourceRect:Rectangle, targetRect:Rectangle):BitmapData {
			if ( targetRect.width > 0 && targetRect.width <= LIMIT && targetRect.height > 0 && targetRect.height <= LIMIT ) {  
				var bitmapdata:BitmapData = new BitmapData(targetRect.width, targetRect.height);
				var transformation:Matrix = new Matrix();
				if ( sourceRect.width <= LIMIT && sourceRect.height <= LIMIT ) {
					transformation.scale(targetRect.width / sourceRect.width, targetRect.height / sourceRect.height);
					var tempbitmapdata:BitmapData = new BitmapData(sourceRect.width, sourceRect.height);
					for ( var sector:int = 0; sector < dim; ++sector ) {
						var intersection:Rectangle = rectangles[sector].intersection(sourceRect);
						if ( !intersection.equals(new Rectangle(0, 0, 0, 0)) ) {
							var x:Number = intersection.topLeft.x - sourceRect.topLeft.x;
							var y:Number = intersection.topLeft.y - sourceRect.topLeft.y;							
							tempbitmapdata.copyPixels(bitmaps[sector], intersection, new Point(x, y));
						}
					}
					bitmapdata.draw(tempbitmapdata, transformation);
					tempbitmapdata.dispose();
				}
				return bitmapdata.clone();
				bitmapdata.dispose();
			}
			return null;
		}
		*/
		
		public function getData(sourceRect:Rectangle, targetRect:Rectangle):BitmapData {
			if ( targetRect.width > 0 && targetRect.width <= LIMIT && targetRect.height > 0 && targetRect.height <= LIMIT ) {  
				var bitmapdata:BitmapData = new BitmapData(targetRect.width, targetRect.height);
				var transformation:Matrix = new Matrix();
				var scalex:Number = targetRect.width / sourceRect.width;
				var scaley:Number = targetRect.height / sourceRect.height;
				transformation.scale(scalex, scaley);
	
				var dimxp:Number = int(sourceRect.width / LIMIT) + 1;
				var dimyp:Number = int(sourceRect.height / LIMIT) + 1;

				for ( var x:int = 0; x < dimxp; ++x ) {
					for ( var y:int = 0; y < dimyp; ++y ) {
						var w:Number = LIMIT;
						var h:Number = LIMIT;
						if ( x == dimxp - 1 ) {
							w = sourceRect.width % LIMIT;
						}
						if ( y == dimyp - 1 ) {
							h = sourceRect.height % LIMIT;
						}
						var tempbitmapdata:BitmapData = new BitmapData(w, h);
						var rectangle:Rectangle = new Rectangle(sourceRect.x + x * LIMIT, sourceRect.y + y * LIMIT, w, h)					
					
						for ( var sector:int = 0; sector < dim; ++sector ) {
							var intersection:Rectangle = rectangle.intersection(rectangles[sector]);
							if ( !intersection.equals(new Rectangle(0, 0, 0, 0)) ) {
								var xp:Number = intersection.topLeft.x - rectangle.topLeft.x;
								var yp:Number = intersection.topLeft.y - rectangle.topLeft.y;
								var dintersection:Rectangle = intersection.clone();
								dintersection.offset(-rectangles[sector].x, -rectangles[sector].y);
								tempbitmapdata.copyPixels(bitmaps[sector], dintersection, new Point(xp, yp));
							}
						}
						
						var tempbitmapdatascaled:BitmapData = new BitmapData(w * scalex, h * scaley);
						tempbitmapdatascaled.draw(tempbitmapdata, transformation);
						
						rectangle = new Rectangle(0, 0, tempbitmapdatascaled.width, tempbitmapdatascaled.height);
						var pt:Point = new Point(x * LIMIT * scalex, y * LIMIT * scaley);
						bitmapdata.copyPixels(tempbitmapdatascaled, rectangle, pt);
						
						tempbitmapdata.dispose();
						tempbitmapdatascaled.dispose();
						
					}
				}
				return bitmapdata.clone();
				bitmapdata.dispose();
			}
			return null;
		}

		public function lock(): void {
			for ( var sector:int = 0; sector < dim; ++sector ) {
				bitmaps[sector].lock();
			}
		}

		public function unlock(): void {
			for ( var sector:int = 0; sector < dim; ++sector ) {
				bitmaps[sector].unlock();
			}
		}
		
		private function map(x:Number, y:Number): Array {
			var sector:int = int(y / LIMIT) * dimx + int(x / LIMIT);
			var xp:Number = x % LIMIT;
			var yp:Number = y % LIMIT;
			return [sector, xp, yp];
		}

	}
}