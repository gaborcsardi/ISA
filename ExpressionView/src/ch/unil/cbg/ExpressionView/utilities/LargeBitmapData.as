package ch.unil.cbg.ExpressionView.utilities {
	
	import __AS3__.vec.Vector;
	
	import flash.display.BitmapData;
	import flash.geom.Matrix;
	import flash.geom.Rectangle;
	import flash.geom.Point;

	public class LargeBitmapData {
	
		private const LIMIT:Number = 8000;
	
		private var bitmaps:Vector.<BitmapData>;
		private var rectangles:Vector.<Rectangle>;
		private var dimx:int;
		private var dimy:int;
		private var dim:int;
		
		public var width:Number;
		public var height:Number;
		//private var restx:Number;
		//private var resty:Number;
	
		public function LargeBitmapData(_width:Number = 0, _height:Number = 0) {
			width = _width;
			height = _height;
			dimx = int(width / LIMIT) + 1;
			dimy = int(height / LIMIT) + 1;
			dim = dimx * dimy;
			var restx:Number = height % LIMIT;
			var resty:Number = width % LIMIT;
			bitmaps = new Vector.<BitmapData>(dim, false);
			rectangles = new Vector.<Rectangle>(dim, false);
			for ( var x:int = 0; x < dimx; ++x ) {
				for ( var y:int = 0; y < dimy; ++y ) {
					var k:int = y * dimx + x;
					var w:Number = LIMIT;
					var h:Number = LIMIT;
					if ( x == dimx - 1 ) {
						w = restx;
					}
					if ( y == dimy - 1 ) {
						h = resty;
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
		
		public function getData(sourceRect:Rectangle, targetRect:Rectangle):BitmapData {
			if ( targetRect.width > 0 && targetRect.width <= LIMIT && targetRect.height > 0 && targetRect.height <= LIMIT ) {  
				var bitmapdata:BitmapData = new BitmapData(targetRect.width, targetRect.height);
				var transformation:Matrix = new Matrix();
				transformation.scale(sourceRect.width / targetRect.width, sourceRect.height / targetRect.height); 
				if ( sourceRect.width <= LIMIT && sourceRect.height <= LIMIT ) {
					var tempbitmapdata:BitmapData = new BitmapData(sourceRect.width, sourceRect.height);
					for ( var sector:int = 0; sector < dim; ++sector ) {
						var intersection:Rectangle = sourceRect.intersection(rectangles[sector]); 
						if ( !intersection.equals(new Rectangle(0, 0, 0, 0)) ) {
							var x:Number = intersection.topLeft.x - sourceRect.topLeft.x;
							var y:Number = intersection.topLeft.y - sourceRect.topLeft.y;							
							tempbitmapdata.copyPixels(bitmaps[sector], intersection, new Point(x, y));
						}
					}
					bitmapdata.draw(tempbitmapdata, transformation);
					tempbitmapdata.dispose();
				} else {
					
				}
				return bitmapdata;
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