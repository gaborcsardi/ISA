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

package ch.unil.cbg.ExpressionView.events {
	
	import flash.events.Event;
	
	public class MenuEvent extends Event {

		// menu item
		public static const OPEN:String = "Open";
		
		public static const MODE:String = "Behavior";
		
		public static const HIGHLIGHTING:String = "Highlighting";
		public static const OUTLINE:String = "Outline";
		public static const FILLING:String = "Filling";
		public static const ALPHA:String = "Alpha";
		
		public static const PDF_EXPORT:String = "PDFExport";
		public static const EXCEL_EXPORT:String = "ExcelExport";
		
		public static const PANELS:String = "Panels";
		
		public static const DEFAULT_POSITIONS:String = "DefaultPositions";
		public static const FULLSCREEN:String = "Fullscreen";

		// data
		public var data:Array;		
		
		public function MenuEvent(_type:String, _data:Array=null, _bubbles:Boolean=true, _cancelable:Boolean=true) {
			super(_type, _bubbles, _cancelable);
			data = _data;
		}
		
		override public function clone(): Event {
			return new MenuEvent(type, data, bubbles, cancelable);
		}

	}
}