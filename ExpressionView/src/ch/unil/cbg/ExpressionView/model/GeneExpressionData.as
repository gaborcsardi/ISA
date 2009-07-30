package ch.unil.cbg.ExpressionView.model {
	
	import __AS3__.vec.Vector;
	
	import ch.unil.cbg.ExpressionView.events.*;
	import ch.unil.cbg.ExpressionView.utilities.LargeBitmapData;
	
	import flash.display.BitmapData;
	import flash.events.EventDispatcher;
	import flash.geom.Rectangle;
	import flash.utils.ByteArray;
	import flash.utils.setTimeout;
	
	import mx.collections.XMLListCollection;
	
	[Event(name=UpdateStatusBarEvent.UPDATESTATUSBAREVENT, type="ch.unil.cbg.expressionview.events.UpdateStatusBarEvent")];
	[Event(name=GEDCompleteEvent.GEDCOMPLETEEVENT, type="ch.unil.cbg.expressionview.events.GEDCcmpleteEvent")];
	public class GeneExpressionData extends EventDispatcher {
		
		public var nModules:int;
		public var Modules:XMLListCollection;

		private var nGenes:int;
		private var nSamples:int;
		
		public var shortLabelsGene:Array;
		public var shortLabelsSample:Array;
		public var shortLabelsModule:Array;
		public var longLabelsGene:Array;
		public var longLabelsSample:Array;
		public var longLabelsModule:Array;
		
		public var ModulesColors:Vector.<Array>;

		private var Data:ByteArray;
		private var ModularData:Vector.<GeneExpressionModule>;
		
		private var gebitmapdata:LargeBitmapData;
		private var modulesbitmapdata:LargeBitmapData;
		//private var templatebitmapdata:BitmapDataUnlimited;
		
		private var ModulesLookup:Vector.<Array>;
		private var ModulesLookupGenes:Vector.<Array>;
		private var ModulesLookupSamples:Vector.<Array>;
		private var ModulesLookupModules:Vector.<Array>;
		
		private var GenesLookup:Vector.<Array>;
		private var SamplesLookup:Vector.<Array>;
				
		public function GeneExpressionData() {
			super();
			nModules = 0;
			Modules = new XMLListCollection();

			nGenes = 0;
			nSamples = 0;
					
			Data = new ByteArray();
			
			ModularData = new Vector.<GeneExpressionModule>();
												
			ModulesLookup = new Vector.<Array>();
			ModulesLookupGenes = new Vector.<Array>();
			ModulesLookupSamples = new Vector.<Array>();
			ModulesLookupModules = new Vector.<Array>();
			
			GenesLookup = new Vector.<Array>();
			SamplesLookup = new Vector.<Array>();
		}


		public function initialize(bytes: ByteArray): void  {		

			XML.ignoreWhitespace = true;

			bytes.position = 19;
			nGenes = bytes.readInt();
			nSamples = bytes.readInt();
			nModules = bytes.readInt();
						
			bytes.readBytes(Data, 0, (nGenes * nSamples)*4);
			
			// get XML Data
			XML.ignoreWhitespace = true;
			bytes.position = (nGenes * nSamples + 3) * 4 + 19;
			var length:int = bytes.length - bytes.position;
			var xmldata:XML = new XML(bytes.readUTFBytes(length));

			Modules = new XMLListCollection(xmldata.modules.module);
			ModularData = new Vector.<GeneExpressionModule>(nModules+1, true);
			ModulesColors = new Vector.<Array>(nModules+1, true);
			for ( var i:int = 0; i <= nModules; ++i ) {
				ModularData[i] = new GeneExpressionModule();
				ModulesColors[i]  = [hsv2rgb(i / nModules * 360, 100, 60), hsv2rgb(i / nModules * 360, 100, 100)];
			}
			
			// set labels
			shortLabelsGene = [];
			longLabelsGene = [];
			var shortgenetags:XMLListCollection = new XMLListCollection(xmldata.genes.shortgenetags.genetag);
			var longgenetags:XMLListCollection = new XMLListCollection(xmldata.genes.longgenetags.genetag);
			for ( var i:int = 0; i < shortgenetags.length; i++ ) {
				shortLabelsGene.push(shortgenetags[i]);
			}
			for ( i = 0; i < longgenetags.length; i++ ) {
				longLabelsGene.push(longgenetags[i]);
			}
			shortLabelsSample = [];
			longLabelsSample = [];
			var shortsampletags:XMLListCollection = new XMLListCollection(xmldata.samples.shortsampletags.sampletag);
			var longsampletags:XMLListCollection = new XMLListCollection(xmldata.samples.longsampletags.sampletag);
			for ( i = 0; i < shortsampletags.length; i++ ) {
				shortLabelsSample.push(shortsampletags[i]);
			}
			for ( i = 0; i < longsampletags.length; i++ ) {
				longLabelsSample.push(longsampletags[i]);
			}
			shortLabelsModule = [];
			longLabelsModule = [];
			var shortmoduletags:XMLListCollection = new XMLListCollection(xmldata.modules.shortmoduletags.moduletag);
			var longmoduletags:XMLListCollection = new XMLListCollection(xmldata.modules.longmoduletags.moduletag);
			for ( i = 0; i < shortmoduletags.length; i++ ) {
				shortLabelsModule.push(shortmoduletags[i]);
			}
			for ( i = 0; i < longmoduletags.length; i++ ) {
				longLabelsModule.push(longmoduletags[i]);
			}

			// set modularData[0]
			ModularData[0].nGenes = nGenes;
			ModularData[0].Genes = new XMLListCollection(xmldata.genes.gene);
			ModularData[0].nSamples = nSamples;
			ModularData[0].Samples = new XMLListCollection(xmldata.samples.sample);

			ModulesLookup = new Vector.<Array>(nGenes * nSamples, true);
			for ( var i:int = 0; i < ModulesLookup.length; ++i ) { ModulesLookup[i] = []; }
			
			ModulesLookupGenes = new Vector.<Array>(nGenes, true);
			for ( var i:int = 0; i < ModulesLookupGenes.length; ++i ) { ModulesLookupGenes[i] = []; }

			ModulesLookupSamples = new Vector.<Array>(nSamples, true);
			for ( var i:int = 0; i < ModulesLookupSamples.length; ++i ) { ModulesLookupSamples[i] = []; }

			ModulesLookupModules = new Vector.<Array>(nModules+1, true);
			for ( var i:int = 0; i < ModulesLookupModules.length; ++i ) { ModulesLookupModules[i] = []; }

			GenesLookup = new Vector.<Array>(nModules+1, true);
			SamplesLookup = new Vector.<Array>(nModules+1, true);			
			ModularData[0].ModulesRectangles = new Vector.<Array>(nModules+1, true);
			ModularData[0].ModulesOutlines = [];

			setTimeout(treatModules, 10, 1);
			
		}

		public function getModule(module:int): GeneExpressionModule {
			if ( nModules > 0 && module >= 0 && module <= nModules ) {
				if ( ModularData[module].nGenes == 0 ) {
					generateModule(module);
				}
				return ModularData[module];
			}
			return new GeneExpressionModule();
		}
		
		public function generatedModules(): Array {
			var module:int;
			var whichModules:Array = [];
			for ( module = 0; module <= nModules; ++module ) {
				if ( ModularData[module].nGenes != 0 ) {
					whichModules.push(module);
				}
			}
			return whichModules;
		}

		private function generateModule(module:int): void {
						
			var global:GeneExpressionModule = ModularData[0];
			var newmodule:GeneExpressionModule = new GeneExpressionModule();
			
			var genes:Array = GenesLookup[module];
			var samples:Array = SamplesLookup[module];
			var ngenes:int = genes.length;
			var nsamples:int = samples.length;
			
			newmodule.nGenes = ngenes;
			//var Genes:XMLListCollection = new XMLListCollection
			for ( var i:int = 0; i < genes.length; ++i ) {
				newmodule.Genes.addItem(global.Genes[genes[i]-1]);
			}			
			newmodule.nSamples = nsamples;
			for ( var i:int = 0; i < samples.length; ++i ) {
				newmodule.Samples.addItem(global.Samples[samples[i]-1]);
			}
			
			// get ModulesRectangles
			newmodule.ModulesRectangles = new Vector.<Array>(nModules + 1, true);
			newmodule.ModulesOutlines = [];
			for ( var m:int = 0; m < ModulesLookupModules[module].length; ++m ) {
								
				var modulep:int = ModulesLookupModules[module][m];
				var genesp:Array = GenesLookup[modulep];
				var samplesp:Array = SamplesLookup[modulep];
				
				var rectxleft:Array = []; var rectxright:Array = [];
				var rectytop:Array = []; var rectybottom:Array = [];
								
				// determine rectangles
				var oldgene:int = genes.indexOf(genesp[0], 0)
				for ( var genespp:int = 1; genespp < genesp.length; ++genespp ) {
					var gene:int = genes.indexOf(genesp[genespp], oldgene);
					if ( gene > oldgene + 1 ) {
						rectxright.push(oldgene);
						rectxleft.push(gene);
						oldgene = gene;
					};
				};

				var oldsample:int = samples.indexOf(samplesp[0], 0)
				for ( var samplespp:int = 1; samplespp < samplesp.length; ++samplespp ) {
					var sample:int = samples.indexOf(samplesp[samplespp], oldsample);
					if ( sample > oldsample + 1 ) {
						rectxright.push(oldsample);
						rectxleft.push(sample);
						oldsample = sample;
					};
				};
				
				newmodule.ModulesRectangles[modulep] = [];
				var maxarea:int = 0;
				var bestrect:int = 0;
				for ( var rectx:int = 0; rectx < rectxright.length; ++rectx ) {
					for ( var recty:int = 0; recty < rectytop.length; ++recty ) {
						var x:int = rectxleft[rectx] - 1;
						var y:int = rectytop[recty] - 1;
						var dx:int = rectxright[rectx] - x;
						var dy:int = rectybottom[recty] - y;
						var area:int = dx * dy;
						if ( area > maxarea ) { 
							maxarea = area;
							bestrect = newmodule.ModulesRectangles[module].length;
						}
						newmodule.ModulesRectangles[modulep].push(new Rectangle(x, y, dx, dy));
					}
				}
				newmodule.ModulesOutlines.push(bestrect);

			}

    		var gebitmapdata:LargeBitmapData = new LargeBitmapData(ngenes, nsamples);
    		var modulesbitmapdata:LargeBitmapData = new LargeBitmapData(ngenes, nsamples);
    		gebitmapdata.lock();
    		modulesbitmapdata.lock();
			for ( var genep:int = 0; genep < ngenes; ++genep ) {
				var gene:int = genes[genep];
        		for ( var samplep:int = 0; samplep < nsamples; ++samplep ) {
        			var sample:int = samples[samplep];
					var value:Number = global.GEImage.getPixel(gene-1, sample-1);
					gebitmapdata.setPixel(genep, samplep, value);					
					value = global.ModulesImage.getPixel(gene-1, sample-1);
					var k:int = (sample - 1) * nGenes + gene - 1;
					if ( ModulesLookup[k].length > 0 ) {
						var color:uint = ModulesColors[ModulesLookup[k][ModulesLookup[k].length-1]][0];
						if ( color != ModulesColors[module][0] ) {
							modulesbitmapdata.setPixel(genep, samplep, color);
						}
						if ( color == ModulesColors[module][0] && ModulesLookup[k].length > 1 ) { 
							color = ModulesColors[ModulesLookup[k][ModulesLookup[k].length-2]][0];
							modulesbitmapdata.setPixel(genep, samplep, color);
						}						
					}
				}			
			}			      
			gebitmapdata.unlock();
			modulesbitmapdata.unlock();
			
			newmodule.GEImage = gebitmapdata;
			newmodule.ModulesImage = modulesbitmapdata;
			
			ModularData[module] = newmodule;
       					
		}
		
		public function getInfo(module:int, gene:int, sample:int): Array {
			if ( ModularData[module].nGenes != 0 && gene >= 0 && gene < ModularData[module].nGenes && 
					sample >= 0 && sample < ModularData[module].nSamples ) {
				var geneDescription:XML = ModularData[module].Genes.source[gene];
				var sampleDescription:XML = ModularData[module].Samples.source[sample];
				var genep:int = gene + 1;
				var samplep:int = sample + 1;
				if ( module != 0 ) {
					genep = GenesLookup[module][gene];
					samplep = SamplesLookup[module][sample];
				}
				var k:int = (samplep-1) * nGenes + genep - 1;
				var modules:Array = [];
				if ( k >= 0 && k < ModulesLookup.length ) {
					modules = ModulesLookup[k];
				}
				Data.position = k * 4;
				var data:Number = Data.readFloat();
				return new Array(geneDescription, sampleDescription, modules, data);
			}
			return null;
		}
		
		
		private function treatModules(modulestart:int):void {
			
			var module:int;
			for ( module = modulestart; module <= modulestart + 49; module++ ) {
				
				if ( module == nModules + 1 ) {
					setTimeout(initTreatBitmap, 10);
					return;
				}

	        	var string:String = Modules.source[module-1].containedgenes.toString();
		   		var genes:Array = string.split(", ");
		   		genes.sort(Array.NUMERIC);
				string = Modules.source[module-1].containedsamples.toString();
				var samples:Array = string.split(", ");
				samples.sort(Array.NUMERIC);
				string = Modules.source[module-1].intersectingmodules.toString();
				var modules:Array = string.split(", ");
				modules.sort(Array.NUMERIC);
									
				var rectxleft:Array = []; var rectxright:Array = [];
				var rectytop:Array = []; var rectybottom:Array = [];				

				// cast string to int and determine rectangles
				GenesLookup[module] = [];
				var oldgene:int = int(genes[0]);
				rectxleft.push(oldgene);
				for ( var genep:int = 0; genep < genes.length; ++genep ) {
					var gene:int = int(genes[genep]);
					GenesLookup[module].push(gene);
					ModulesLookupGenes[genep].push(module);
					if ( gene > oldgene + 1 ) {
						rectxright.push(oldgene);
						rectxleft.push(gene);
					}
					oldgene = gene;
				};
				rectxright.push(oldgene);
	
				SamplesLookup[module] = [];
				var oldsample:int = int(samples[0]);
				rectytop.push(oldsample);
				for ( var samplep:int = 0; samplep < samples.length; ++samplep ) {
					var sample:int = int(samples[samplep]);
					SamplesLookup[module].push(sample);
					ModulesLookupSamples[samplep].push(module);
					if ( sample > oldsample + 1 ) {
						rectybottom.push(oldsample);
						rectytop.push(sample);
					}
					oldsample = sample;
				};
				rectybottom.push(oldsample);
				
				ModularData[0].ModulesRectangles[module] = [];
				var maxarea:int = 0;
				var bestrect:int = 0;
				for ( var rectx:int = 0; rectx < rectxright.length; ++rectx ) {
					for ( var recty:int = 0; recty < rectytop.length; ++recty ) {
						var x:int = rectxleft[rectx] - 1;
						var y:int = rectytop[recty] - 1;
						var dx:int = rectxright[rectx] - x;
						var dy:int = rectybottom[recty] - y;
						var area:int = dx * dy;
						if ( area > maxarea ) { 
							maxarea = area;
							bestrect = ModularData[0].ModulesRectangles[module].length;
						}
						ModularData[0].ModulesRectangles[module].push(new Rectangle(x, y, dx, dy));
					}
				}
				ModularData[0].ModulesOutlines.push(bestrect);			
			
				for ( var genep:int = 0; genep < GenesLookup[module].length; ++genep ) {
					var gene:int = GenesLookup[module][genep];
					for ( var samplep:int = 0; samplep < SamplesLookup[module].length; ++samplep ) {
						var sample:int = SamplesLookup[module][samplep];
						var k:int = (sample-1) * nGenes + gene - 1;
						if ( ModulesLookup[k] == null ) { 
							ModulesLookup[k] = [module]; 
						} else {
							ModulesLookup[k].push(module);							
						}
					}
				}
	
				for ( var modulep:int = 0; modulep < modules.length; ++modulep ) {
					var m:int = int(modules[modulep]);
					if ( m != 0 ) {
						ModulesLookupModules[module].push(m);
					}
				}
								
			}					
			
			this.dispatchEvent(new UpdateStatusBarEvent("reading module " + module + " of " + nModules + "."));			
			setTimeout(treatModules, 10, module);

		}
		
		private function initTreatBitmap():void {
			
			// set Data and get global Bitmap
			gebitmapdata = new LargeBitmapData(nGenes, nSamples);
			modulesbitmapdata = new LargeBitmapData(nGenes, nSamples);
						
			gebitmapdata.lock();
			modulesbitmapdata.lock();
			Data.position = 0;
		
			setTimeout(treatBitmap, 10, 1);
				
		}

		private function treatBitmap(startgene:int):void {
						
			var gene:int;
			for ( gene = startgene; gene <= startgene + 200; ++gene ) {

				if ( gene == nGenes ) {
					setTimeout(finishTreatBitmap, 10);
					return;
				}
						
				for ( var sample:int = 1; sample <= nSamples; ++sample ) {
					var value:Number = Data.readFloat();
					var red:Number; var green:Number;
					if ( value >= 0 ) {
						red = value * 255;
						green = 0;
					} else {
						red = 0;
						green = -value * 255;
					}
					gebitmapdata.setPixel(gene-1, sample-1, (red<<16) + (green<<8) + 0);
					
					var k:int = (sample-1) * nGenes + gene - 1;
					if ( ModulesLookup[k].length > 0 ) {
						var color:uint = ModulesColors[ModulesLookup[k][ModulesLookup[k].length-1]][0];
						modulesbitmapdata.setPixel(gene-1, sample-1, color);
					}

				}				
			
			}
			
			this.dispatchEvent(new UpdateStatusBarEvent("generating bitmap " + int(gene/nGenes*100) + " %."));
			setTimeout(treatBitmap, 10, gene);
		
		}
		
		private function hsv2rgb(hue:Number, sat:Number, val:Number):uint {
		    var red, green, blue, i, f, p, q, t;
		    hue %= 360;
		    if(val==0) { 
		    	return ( 0 << 16 | 0 << 8 | 0 );
		    }
		    sat /= 100;
		    val /= 100;
		    hue /= 60;
		    i = Math.floor(hue);
		    f = hue - i;
		    p = val*(1-sat);
		    q = val*(1-(sat*f));
		    t = val*(1-(sat*(1-f)));
		    if (i==0) {red=val; green=t; blue=p;}
		    else if (i==1) {red=q; green=val; blue=p;}
		    else if (i==2) {red=p; green=val; blue=t;}
		    else if (i==3) {red=p; green=q; blue=val;}
		    else if (i==4) {red=t; green=p; blue=val;}
		    else if (i==5) {red=val; green=p; blue=q;}
		    red = Math.floor(red*255);
		    green = Math.floor(green*255);
		    blue = Math.floor(blue*255);
		    return ( red << 16 | green << 8 | blue );
		}
				
		private function finishTreatBitmap(): void {	
			gebitmapdata.unlock();
			modulesbitmapdata.unlock();
			ModularData[0].GEImage = gebitmapdata;
			ModularData[0].ModulesImage = modulesbitmapdata;
			dispatchEvent(new GEDCompleteEvent());
		}
			
			
	}
}