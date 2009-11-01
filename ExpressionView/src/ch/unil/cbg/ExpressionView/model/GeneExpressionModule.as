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

package ch.unil.cbg.ExpressionView.model {
	
	import __AS3__.vec.Vector;
	
	import ch.unil.cbg.ExpressionView.utilities.LargeBitmapData;
	
	import mx.collections.XMLListCollection; 		
	
	/**
	 * Base class for a gene expression module.
	 * Several modules make up the gene expression data.
	 */	
	public class GeneExpressionModule {

		public var nGenes:int;
		public var Genes:XMLListCollection;
		
		public var nSamples:int;
		public var Samples:XMLListCollection;
		
		public var GO:XMLListCollection;
		public var KEGG:XMLListCollection;
				
		public var GEImage:LargeBitmapData;
		public var ModulesImage:LargeBitmapData;

		public var ModulesRectangles:Vector.<Array>;
		public var ModulesOutlines:Vector.<int>;

		public function GeneExpressionModule() {
			nGenes = 0;
			Genes = new XMLListCollection();
			
			nSamples = 0;
			Samples = new XMLListCollection();
			
			ModulesRectangles = new Vector.<Array>();
		}
				
	}
	
}