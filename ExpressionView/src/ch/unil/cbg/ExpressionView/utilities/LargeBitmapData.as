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

package ch.unil.cbg.ExpressionView.utilities {
	
	import __AS3__.vec.Vector;
	
	import flash.display.BitmapData;
	import flash.geom.Matrix;
	import flash.geom.Point;
	import flash.geom.Rectangle;

	/**
	 * The LargeBitmapData class can store bitmapdata of arbitrary size.
	 * 
	 * @author Andreas Lüscher
	 *
	 * @example
	 * The example shows how to use the LargeBitmapData class with a bitmap of 10000 x 10000 pixels.
	 * <div class="listing">
	 * <pre>
	 * 
	 * var width:Number = 10000;
	 * var height:Number = 10000;
	 * largebitmapdata = new LargeBitmapData(width, height);
	 * ...
	 * 
	 * </pre>
	 * </div>
	 * 
	 */
	 public class LargeBitmapData {
	
		private const LIMIT:Number = 8000;
	
		private var bitmaps:Vector.<BitmapData>;
		private var rectangles:Vector.<Rectangle>;
		private var dimx:int;
		private var dimy:int;
		private var dim:int;
		
		public var width:Number;
		public var height:Number;
	
		/**
		* Constructor.
		*/	
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

		/**
		* Set the color of a Pixel.
		* 
		* @param x x-Position
		* @param y y-Position
		* @param value uint representing the color 
		*/	
		public function setPixel(x:Number, y:Number, value:uint):void {
			var position:Array = map(x, y);
			bitmaps[position[0]].setPixel(position[1], position[2], value);
		}


		/**
		* Get the color of a Pixel.
		* 
		* @param x x-Position
		* @param y y-Position
		* 
		* @return uint representing the color  
		*/	
		public function getPixel(x:Number, y:Number):uint {
			var position:Array = map(x, y);
			return bitmaps[position[0]].getPixel(position[1], position[2]);
		}
		
				
		/**
		* Get the rescaled bitmapdata.
		* 
		* @param sourceRect Part of the bitmap you want to scale
		* @param targetRect Target size
		* 
		* @return Bitmapdata containing a rescaled part of the bitmap   
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

		/**
		* Lock largebitmapdata before setting pixels.
		*/
		public function lock(): void {
			for ( var sector:int = 0; sector < dim; ++sector ) {
				bitmaps[sector].lock();
			}
		}

		/**
		* Unlock largebitmapdata after setting pixels.
		*/
		public function unlock(): void {
			for ( var sector:int = 0; sector < dim; ++sector ) {
				bitmaps[sector].unlock();
			}
		}
		
		/**
		* Map the coordinates (x,y) onto the corresponding tiles.
		*
		* @param x x-Position  
		* @param y y-Position
		*
		* @return Array(x',y',sector) containing the corresponding (x',y') coordinates in tile sector.
		*/
		private function map(x:Number, y:Number): Array {
			var sector:int = int(y / LIMIT) * dimx + int(x / LIMIT);
			var xp:Number = x % LIMIT;
			var yp:Number = y % LIMIT;
			return [sector, xp, yp];
		}

	}
}