package ch.unil.cbg.ExpressionView.view {
	
	import __AS3__.vec.Vector;
	
	import ch.unil.cbg.ExpressionView.events.*;
	import ch.unil.cbg.ExpressionView.model.*;
	import ch.unil.cbg.ExpressionView.view.components.*;
	
	import flash.events.MouseEvent;
	import flash.geom.Rectangle;
	import flash.net.FileReference;
	import flash.utils.ByteArray;
	
	import flexlib.containers.SuperTabNavigator;
	
	import mx.containers.Canvas;
	import mx.controls.TextArea;
	import mx.controls.dataGridClasses.DataGridColumn;
	import mx.events.IndexChangedEvent;
	
	import org.alivepdf.colors.RGBColor;
	import org.alivepdf.fonts.FontFamily;
	import org.alivepdf.fonts.Style;
	import org.alivepdf.layout.Orientation;
	import org.alivepdf.layout.Size;
	import org.alivepdf.layout.Unit;
	import org.alivepdf.pdf.PDF;
	import org.alivepdf.saving.Method;

	public class MainCanvas extends Canvas {
		
		private var useDefaultPositions:Boolean;
		
		private var rawged:GeneExpressionData;
		private var ged:GeneExpressionData;
		
		private var selectedMode:int;
		
		private var lastHighlightedModules:Array;
		
		private var gePanel:ResizablePanel;
		private var modulesNavigator:SuperTabNavigator;
		private var openTabs:Vector.<ZoomPanCanvas>;
		private var mapOpenTabs:Vector.<int>;
		
		private var infoPanel:ResizablePanel;
		private var infoContent:TextArea;
		
		private var modulesPanel:ResizablePanel;
		private var modulesSearchableDataGrid:SearchableDataGrid;
		private var genesPanel:ResizablePanel;
		private var genesSearchableDataGrid:SearchableDataGrid;		
		private var samplesPanel:ResizablePanel;
		private var samplesSearchableDataGrid:SearchableDataGrid;
		
		private var gedatainfoPanel:ResizablePanel;
		private var gedatainfoContent:TextArea;

		private var currentalpha:Number;
				
		public function MainCanvas() {
			super();

			ged = new GeneExpressionData();
			lastHighlightedModules = new Array();
			useDefaultPositions = true;
		}
		
		override protected function createChildren(): void {
			
			super.createChildren();
						
			if ( !gePanel ) {
				gePanel = new ResizablePanel();
				gePanel.enableClose = false;
				addChild(gePanel);
				
				if ( !modulesNavigator ) {
					modulesNavigator = new SuperTabNavigator();
					modulesNavigator.addEventListener(IndexChangedEvent.CHANGE, tabChangeHandler);
					gePanel.addChild(modulesNavigator);
					
					openTabs = new Vector.<ZoomPanCanvas>;
					mapOpenTabs = new Vector.<int>;

				}
			}
			
			if ( !infoPanel ) {
				infoPanel = new ResizablePanel();
				infoPanel.title = "Info";
				infoPanel.addEventListener(ResizablePanelEvent.CLOSE, infoPanelCloseHandler);
				addChild(infoPanel);
				
				if ( !infoContent ) {
					infoContent = new TextArea();
					infoContent.setStyle("backgroundAlpha", infoPanel.getStyle("backgroundAlpha"));
					infoPanel.addChild(infoContent);
				}
				
			}
		
			if ( !modulesPanel ) {
				modulesPanel = new ResizablePanel();
				modulesPanel.title = "Modules";
				modulesPanel.addEventListener(ResizablePanelEvent.CLOSE, modulesPanelCloseHandler);
				addChild(modulesPanel);
												
				if ( !modulesSearchableDataGrid ) {
					modulesSearchableDataGrid = new SearchableDataGrid();
					modulesSearchableDataGrid.addEventListener(SearchableDataGridSelectionEvent.ITEM_CLICK, clickModulesHandler);
					modulesSearchableDataGrid.addEventListener(SearchableDataGridSelectionEvent.ITEM_DOUBLE_CLICK, doubleClickModulesHandler);
					modulesPanel.addChild(modulesSearchableDataGrid);
				}
							
			}

			if ( !genesPanel ) {
				genesPanel = new ResizablePanel();
				genesPanel.title = "Genes";
				genesPanel.addEventListener(ResizablePanelEvent.CLOSE, genesPanelCloseHandler);
				addChild(genesPanel);
												
				if ( !genesSearchableDataGrid ) {
					genesSearchableDataGrid = new SearchableDataGrid();
					genesSearchableDataGrid.addEventListener(SearchableDataGridSelectionEvent.ITEM_CLICK, clickGenesHandler);
					genesSearchableDataGrid.addEventListener(SearchableDataGridSelectionEvent.ITEM_DOUBLE_CLICK, doubleClickGenesHandler);
					genesPanel.addChild(genesSearchableDataGrid);
				}			
			}
			
			if ( !samplesPanel ) {
				samplesPanel = new ResizablePanel();
				samplesPanel.title = "Samples";
				samplesPanel.addEventListener(ResizablePanelEvent.CLOSE, samplesPanelCloseHandler);
				addChild(samplesPanel);
												
				if ( !samplesSearchableDataGrid ) {
					samplesSearchableDataGrid = new SearchableDataGrid();
					samplesSearchableDataGrid.addEventListener(SearchableDataGridSelectionEvent.ITEM_CLICK, clickSamplesHandler);
					samplesSearchableDataGrid.addEventListener(SearchableDataGridSelectionEvent.ITEM_DOUBLE_CLICK, doubleClickSamplesHandler);
					samplesPanel.addChild(samplesSearchableDataGrid);
				}			
			}

			if ( !gedatainfoPanel ) {
				gedatainfoPanel = new ResizablePanel();
				gedatainfoPanel.title = "Gene Expression Data Description";
				gedatainfoPanel.visible = false;
				gedatainfoPanel.addEventListener(ResizablePanelEvent.CLOSE, gedatainfoPanelCloseHandler);
				addChild(gedatainfoPanel);
				
				if ( !gedatainfoContent ) {
					gedatainfoContent = new TextArea();
					gedatainfoPanel.addChild(gedatainfoContent);					
				}
			}

			useDefaultPositions = true;
			
			parentApplication.addEventListener(MenuEvent.DEFAULT_POSITIONS, setDefaultPositionsHandler);
			parentApplication.addEventListener(UpdateGEDataEvent.UPDATEGEDATAEVENT, updateGEDataHandler);
			parentApplication.addEventListener(MenuEvent.MODE, modeChangeHandler);
			parentApplication.addEventListener(MenuEvent.PANELS, setPanelVisibilityHandler);
			parentApplication.addEventListener(BroadcastInspectPositionEvent.BROADCASTINSPECTPOSITIONEVENT, broadcastInspectPositionHandler);
			parentApplication.addEventListener(MenuEvent.PDF_EXPORT, pdfExportHandler);
			parentApplication.addEventListener(MenuEvent.ALPHA, alphaSliderChangeHandler);	
			parentApplication.addEventListener(ResizeBrowserEvent.RESIZEBROWSEREVENT, resizeBrowserHandler);
		}
		
		private function gedatainfoPanelCloseHandler(event:ResizablePanelEvent):void {
			parentApplication.dispatchEvent(new MenuEvent(MenuEvent.PANELS, [0, false]));
		}
		private function infoPanelCloseHandler(event:ResizablePanelEvent):void {
			parentApplication.dispatchEvent(new MenuEvent(MenuEvent.PANELS, [1, false]));
		}
		private function modulesPanelCloseHandler(event:ResizablePanelEvent):void {
			parentApplication.dispatchEvent(new MenuEvent(MenuEvent.PANELS, [2, false]));
		}
		private function genesPanelCloseHandler(event:ResizablePanelEvent):void {
			parentApplication.dispatchEvent(new MenuEvent(MenuEvent.PANELS, [3, false]));
		}
		private function samplesPanelCloseHandler(event:ResizablePanelEvent):void {
			parentApplication.dispatchEvent(new MenuEvent(MenuEvent.PANELS, [4, false]));
		}
		
		private function dataDescriptionHandler(event:MenuEvent): void {
			gedatainfoPanel.visible = event.data[0];
		}
		
		private function resizeBrowserHandler(event:ResizeBrowserEvent): void {
			
			var scalex:Number = event.scaleX;
			var scaley:Number = event.scaleY;
			
			if ( isFinite(scalex) && isFinite(scaley) ) {
				gePanel.x *= scalex;
				infoPanel.x *= scalex;
				modulesPanel.x *= scalex;
				genesPanel.x *= scalex;			
				samplesPanel.x *= scalex;
				
				gePanel.width *= scalex;
				infoPanel.width *= scalex;
				modulesPanel.width *= scalex;
				genesPanel.width *= scalex;			
				samplesPanel.width *= scalex;

				gePanel.y *= scaley;
				infoPanel.y *= scaley;
				modulesPanel.y *= scaley;
				genesPanel.y *= scaley;			
				samplesPanel.y *= scaley;

				gePanel.height *= scaley;
				infoPanel.height *= scaley;
				modulesPanel.height *= scaley;
				genesPanel.height *= scaley;			
				samplesPanel.height *= scaley;
			}
			
		}
		
		override protected function updateDisplayList(unscaledWidth:Number, unscaledHeight:Number):void {
			
			super.updateDisplayList(unscaledWidth, unscaledHeight);

			gedatainfoContent.percentWidth = 100;
			gedatainfoContent.percentHeight = 100;
			infoContent.percentWidth = 100;
			infoContent.percentHeight = 100;
			modulesSearchableDataGrid.percentWidth = 100;
			modulesSearchableDataGrid.percentHeight = 100;
			genesSearchableDataGrid.percentWidth = 100;
			genesSearchableDataGrid.percentHeight = 100;
			samplesSearchableDataGrid.percentWidth = 100;
			samplesSearchableDataGrid.percentHeight = 100;

			modulesNavigator.percentWidth = 100;
			modulesNavigator.percentHeight = 100;
				
			for ( var module:int = 0; module < openTabs.length; ++module ) {
				openTabs[module].percentWidth = 100;
				openTabs[module].percentHeight = 100;
			}
			
			if ( useDefaultPositions ) {

				var width:Number = stage.stageWidth * 1/4;
				var x:Number = 3 * width;
				var y:Number = (stage.stageHeight - 70) / 4;

				gePanel.x = 0;
				gePanel.y = 0;
				gePanel.percentWidth = 75;
				gePanel.percentHeight = 100;
				
				infoPanel.x = x;
				infoPanel.y = 0;
				infoPanel.width = width;
				infoPanel.height = y;
				
				modulesPanel.x = x;
				modulesPanel.y = y;
				modulesPanel.width = width;
				modulesPanel.height = y;
							
				genesPanel.x = x;
				genesPanel.y = 2*y;
				genesPanel.width = width;
				genesPanel.height = y;
				
				samplesPanel.x = x;
				samplesPanel.y = 3*y;
				samplesPanel.width = width;
				samplesPanel.height = y;
				
			}
			useDefaultPositions = false;
			
		}

		private function modeChangeHandler(event:MenuEvent): void {
			var mode:int = event.data[0];
			if ( mode != selectedMode ) {
				lastHighlightedModules = new Array();
			}
			selectedMode = mode;
		}
		
		private function tabChangeHandler(event:IndexChangedEvent): void {
			openTabs[event.oldIndex].removeListener();
			openTabs[event.newIndex].addListener();
			dispatchEvent(new MenuEvent(MenuEvent.MODE, [selectedMode]));
		}
		
		private function clickModulesHandler(event:SearchableDataGridSelectionEvent): void {
			var module:int = mapOpenTabs[modulesNavigator.selectedIndex];
			var highlightedRectangles:Array = new Array(ged.nModules + 1);
			for ( var i:int = 0; i < event.selection.length; ++i ) {
				var modulep:int = event.selection[i];
				highlightedRectangles[modulep] = ged.getModule(module).ModulesRectangles[modulep];
			}
			dispatchEvent(new HighlightingEvent(HighlightingEvent.MODULE, [highlightedRectangles]));
		}
		
		private function doubleClickModulesHandler(event:SearchableDataGridSelectionEvent): void {
			
			for ( var i:int = 0; i < event.selection.length; ++i ) {
				var selectedModule:int = event.selection[i];
				var selectedTab:int = mapOpenTabs.indexOf(selectedModule);
				if ( selectedTab == -1 ) {
					var gem:GeneExpressionModule = ged.getModule(selectedModule);
					selectedTab = openTabs.push(new ZoomPanCanvas()) - 1;
					modulesNavigator.addChild(openTabs[selectedTab]);
					openTabs[selectedTab].label = "m" + selectedModule.toString();

					var largestRectangles:Array = new Array(ged.nModules + 1);
					largestRectangles[0] = new Rectangle();
					for ( var module:int = 1; module <= ged.nModules; ++module ) {
						if ( gem.ModulesRectangles[module] != null ) {
							largestRectangles[module] = gem.ModulesRectangles[module][gem.ModulesOutlines[module]];
						} else {
							largestRectangles[module] = new Rectangle();
						}
					}
					openTabs[selectedTab].dataProvider = new Array(gem.GEImage, gem.ModulesImage, largestRectangles, ged.ModulesColors);
					genesSearchableDataGrid.dataProvider = gem.Genes;
					samplesSearchableDataGrid.dataProvider = gem.Samples;	
					mapOpenTabs.push(selectedModule);
				}
				var lastSelectedTab:int = modulesNavigator.selectedIndex; 
				if ( lastSelectedTab != selectedTab ) { 
					openTabs[lastSelectedTab].removeListener();
					openTabs[selectedTab].addListener();
					modulesNavigator.selectedIndex = selectedTab;
				}
				
			}			
		}

		private function clickGenesHandler(event:SearchableDataGridSelectionEvent): void {
			var genes:Array = event.selection;
			genes.sort(Array.NUMERIC);
			
			var rectxleft:Array = []; var rectxright:Array = [];
			var oldgene:int = genes[0];
			rectxleft.push(oldgene);
			for ( var genep:int = 0; genep < genes.length; ++genep ) {
				var gene:int = genes[genep];
				if ( gene > oldgene + 1 ) {
					rectxright.push(oldgene);
					rectxleft.push(gene);
				}
				oldgene = gene;
			};
			rectxright.push(oldgene);
			
			var rectangles:Array = [];
			for ( var i:int = 0; i < rectxleft.length; ++i ) {
				var x:Number = rectxleft[i] - 1;
				var y:Number = 0;
				var dx:Number = rectxright[i] - x;
				var dy:Number = ged.nSamples;
				rectangles.push(new Rectangle(x, y, dx, dy));  
			}
			dispatchEvent(new HighlightingEvent(HighlightingEvent.GENE, [rectangles]));
		}
		private function doubleClickGenesHandler(event:SearchableDataGridSelectionEvent): void {
		}

		private function clickSamplesHandler(event:SearchableDataGridSelectionEvent): void {
			var samples:Array = event.selection;
			samples.sort(Array.NUMERIC);

			var rectytop:Array = []; var rectybottom:Array = [];				
			var oldsample:int = samples[0];
			rectytop.push(oldsample);
			for ( var samplep:int = 0; samplep < samples.length; ++samplep ) {
				var sample:int = samples[samplep];
				if ( sample > oldsample + 1 ) {
					rectybottom.push(oldsample);
					rectytop.push(sample);
				}
				oldsample = sample;
			};
			rectybottom.push(oldsample);

			var rectangles:Array = [];
			for ( var i:int = 0; i < rectytop.length; ++i ) {
				var x:Number = 0;
				var y:Number = rectytop[i] - 1;
				var dx:Number = ged.nGenes;
				var dy:Number = rectybottom[i] - y; 
				rectangles.push(new Rectangle(x, y, dx, dy));  
			}

			dispatchEvent(new HighlightingEvent(HighlightingEvent.SAMPLE, [rectangles]));
		}
		private function doubleClickSamplesHandler(event:SearchableDataGridSelectionEvent): void {
		}

		private function broadcastInspectPositionHandler(event:BroadcastInspectPositionEvent): void {
			var gene:int = event.gene;
			var sample:int = event.sample;
			
			infoContent.text = "";
			var module:int = mapOpenTabs[modulesNavigator.selectedIndex]
			// Array returned is Array(geneDescription, sampleDescription, modules, data, modulesRectangles);
			var infoArray:Array = ged.getInfo(module, gene, sample);
			if ( infoArray != null ) {
				var modules:Array = infoArray[2];
				// update infoPanel
				var infoString:String = "";
				if ( infoArray.length != 0 ) {
					infoString = infoString + "Gene: " + infoArray[0].genetag1;
					infoString = infoString + "\nSample: " + infoArray[1].sampletag1;
					infoString = infoString + "\nModules: " + infoArray[2];
					infoString = infoString + "\nData: " + infoArray[3];
				}
				infoContent.text = infoString;
	
				// highlight module
				if ( lastHighlightedModules != modules ) {
					var highlightedRectangles:Array = new Array(ged.nModules + 1);
					for ( var modulep:int = 0; modulep < modules.length; ++modulep ) {
						if ( modules[modulep] != module ) {
							highlightedRectangles[modules[modulep]] = ged.getModule(module).ModulesRectangles[modules[modulep]];
						}
					}
					dispatchEvent(new HighlightingEvent(HighlightingEvent.MODULE, [highlightedRectangles]));
					lastHighlightedModules = modules;
				}
			
			} else {
				infoContent.text = "";
				dispatchEvent(new HighlightingEvent(HighlightingEvent.MODULE, [new Array(ged.nModules + 1)]));
			}
		}
		
		private function setDefaultPositionsHandler(event:MenuEvent): void {
			useDefaultPositions = true;
			invalidateDisplayList();
		}
		
		private function setPanelVisibilityHandler(event:MenuEvent): void {
			var panel:int = event.data[0]
			var visible:Boolean = event.data[1];
			
			switch (panel) {
				case 0:
					if ( gedatainfoContent.htmlText == "" ) {
						generategedatainfo();
					}
					gedatainfoPanel.visible = visible;
					break;
				case 1:
					infoPanel.visible = visible;
					break;
				case 2:
					modulesPanel.visible = visible;
					break;
				case 3:
					genesPanel.visible = visible;
					break;
				case 4:
					samplesPanel.visible = visible;
					break;
			}			
		}
		
		private function updateGEDataHandler(event:UpdateGEDataEvent): void {
			rawged = event.data[0]
			ged = event.data[1];
			var gem:GeneExpressionModule = ged.getModule(0);

			modulesSearchableDataGrid.dataProvider = ged.Modules;
			genesSearchableDataGrid.dataProvider = gem.Genes;
			samplesSearchableDataGrid.dataProvider = gem.Samples;
			var temp:Array = [];
			for ( var i:int = 1; i < ged.shortLabelsGene.length; i++ ) {
				var column:DataGridColumn = new DataGridColumn();
				column.dataField = "genetag" + i;
				column.headerText = ged.shortLabelsGene[i];
				temp.push(column)
			}
			genesSearchableDataGrid.columns = temp;
			temp = [];
			for ( i = 1; i < ged.shortLabelsSample.length; i++ ) {
				column = new DataGridColumn();
				column.dataField = "sampletag" + i;
				column.headerText = ged.shortLabelsSample[i];
				temp.push(column)
			}
			samplesSearchableDataGrid.columns = temp;
			temp = [];
			for ( i = 1; i < ged.shortLabelsModule.length; i++ ) {
				column = new DataGridColumn();
				column.dataField = "moduletag" + i;
				column.headerText = ged.shortLabelsModule[i];
				temp.push(column)
			}
			modulesSearchableDataGrid.columns = temp;

			modulesNavigator.removeAllChildren();
			
			openTabs = new Vector.<ZoomPanCanvas>;					
			var selectedTab:int = openTabs.push(new ZoomPanCanvas()) - 1;
			modulesNavigator.addChild(openTabs[selectedTab]);

			var largestRectangles:Array = new Array(ged.nModules + 1);
			largestRectangles[0] = new Rectangle();
			for ( var module:int = 1; module <= ged.nModules; ++module ) {
				largestRectangles[module] = gem.ModulesRectangles[module][gem.ModulesOutlines[module]];
			}
			openTabs[selectedTab].label = "Global";
			openTabs[selectedTab].dataProvider = new Array(gem.GEImage, gem.ModulesImage, largestRectangles, ged.ModulesColors);
			openTabs[selectedTab].addListener();
			openTabs[selectedTab].addEventListener(MouseEvent.ROLL_OUT, rollOutHandler);

			mapOpenTabs = new Vector.<int>;
			mapOpenTabs.push(0);				
		}
		
		private function rollOutHandler(event:MouseEvent): void {
			dispatchEvent(new HighlightingEvent(HighlightingEvent.MODULE, [[]]))
		}
		
		private function pdfExportHandler(event:MenuEvent): void {
				
			var myPDF:PDF;
			myPDF = new PDF(Orientation.PORTRAIT, Unit.MM, Size.A4);
			myPDF.addPage();
			myPDF.textStyle(new RGBColor(10));
			myPDF.setFont(FontFamily.HELVETICA, Style.BOLD);
			myPDF.setFontSize(14);
			myPDF.addText("ExpressionView Export", 10, 10);
			myPDF.addImage(openTabs[modulesNavigator.selectedIndex].currentgeimage,10,20,190,0);
			myPDF.addImage(openTabs[modulesNavigator.selectedIndex].currentmodulesimage,10,20,190,0,null,100,currentalpha);

			var bytes:ByteArray = myPDF.save(Method.LOCAL);
			var file:FileReference = new FileReference();
			file.save(bytes);
	
		}
		
		private function alphaSliderChangeHandler(event:MenuEvent): void {
			currentalpha = event.data[0];
		}
		
		private function generategedatainfo(): void {
			if ( ged.XMLData.experimentdata.title != "" ) {
				gedatainfoContent.htmlText = "<b>" + ged.XMLData.experimentdata.title + "</b><br><br>";
			} 
			if ( ged.XMLData.experimentdata.name != "" ) {
				gedatainfoContent.htmlText += ged.XMLData.experimentdata.name + "<br><br>";
			} 
			if ( ged.XMLData.experimentdata.lab != "" ) {
				gedatainfoContent.htmlText += "<i>" + ged.XMLData.experimentdata.lab + "</i><br><br>";
			} 
			if ( ged.XMLData.experimentdata.abstract != "" ) {
				gedatainfoContent.htmlText += "<p>" + ged.XMLData.experimentdata.abstract + "</p><br><br>";
			}
			if ( ged.XMLData.experimentdata.url != "" ) {
				gedatainfoContent.htmlText += ged.XMLData.experimentdata.url + "<br>";
			}
			if ( ged.XMLData.experimentdata.annotation != "" ) {
				gedatainfoContent.htmlText += "Annotation: " + ged.XMLData.experimentdata.annotation;
			}
		}
		
	}
}