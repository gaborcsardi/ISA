package ch.unil.cbg.ExpressionView.model {
	
	import __AS3__.vec.Vector;
	
	import flash.display.Bitmap;
	import flash.utils.ByteArray;
	
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
				
		public var Image:Bitmap;
		public var ModulesImage:Bitmap;

		public var ModulesRectangles:Vector.<Array>;
		public var ModulesOutlines:Array;

		public function GeneExpressionModule() {
			nGenes = 0;
			Genes = new XMLListCollection();
			
			nSamples = 0;
			Samples = new XMLListCollection();
			
			Image = new Bitmap();
			ModulesImage = new Bitmap();
			
			ModulesRectangles = new Vector.<Array>();
		}
				
	}
	
}