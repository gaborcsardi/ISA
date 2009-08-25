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