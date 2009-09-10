package ch.unil.cbg.ExpressionView.view {
	
	import __AS3__.vec.Vector;
	
	import ch.unil.cbg.ExpressionView.events.*;
	import ch.unil.cbg.ExpressionView.model.*;
	import ch.unil.cbg.ExpressionView.view.components.*;
	
	import flash.display.Bitmap;
	import flash.events.MouseEvent;
	import flash.geom.Rectangle;
	import flash.net.FileReference;
	import flash.utils.ByteArray;
	
	import mx.collections.XMLListCollection;
	import mx.containers.Canvas;
	import mx.containers.HDividedBox;
	import mx.containers.Panel;
	import mx.containers.TabNavigator;
	import mx.containers.VDividedBox;
	import mx.controls.Alert;
	import mx.controls.Label;
	import mx.controls.TextArea;
	import mx.controls.dataGridClasses.DataGridColumn;
	import mx.core.ClassFactory;
	import mx.events.IndexChangedEvent;
	import mx.utils.ObjectUtil;
	
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
		private var scoreColumnsVisible:Boolean;
		
		private var rawged:GeneExpressionData;
		private var ged:GeneExpressionData;
		
		private var selectedMode:int;
		private var selectedAlpha:Number = 0.2;
		
		private var lastHighlightedModules:Array;
		
		private var divider:HDividedBox;
		
		private var gePanel:Panel;
		private var modulesNavigator:ClosableTabNavigator;
		private var openTabs:Vector.<ZoomPanCanvas>;
		private var mapOpenTabs:Vector.<int>;
		
		private var infoPanel:Panel;
		private var infoDivider:VDividedBox;
		private var infoContent:TextArea;
		private var infoNavigator:TabNavigator;
		
		private var modulesSearchableDataGrid:SearchableDataGrid;
		private var genesSearchableDataGrid:SearchableDataGrid;		
		private var samplesSearchableDataGrid:SearchableDataGrid;
		private var GOSearchableDataGrid:SearchableDataGrid;
		private var KEGGSearchableDataGrid:SearchableDataGrid;
		private var experimentData:Canvas;
		private var experimentDataContent:TextArea

		public function MainCanvas() {
			super();

			ged = new GeneExpressionData();
			lastHighlightedModules = new Array();
			useDefaultPositions = true;
		}
		
		override protected function createChildren(): void {
			
			super.createChildren();
						
			if ( !divider ) {
				divider = new HDividedBox();
				divider.liveDragging = false;
				addChild(divider);
				
			}
			if ( !gePanel ) {
				gePanel = new Panel();
				divider.addChild(gePanel);
				
				if ( !modulesNavigator ) {
					modulesNavigator = new ClosableTabNavigator();
					modulesNavigator.addEventListener(IndexChangedEvent.CHANGE, tabChangeHandler);
					modulesNavigator.addEventListener(ClosableTabNavigatorEvent.CLOSE, tabCloseHandler);
					modulesNavigator.addEventListener(IndexChangedEvent.CHILD_INDEX_CHANGE, tabReorderHandler);
					gePanel.addChild(modulesNavigator);
					
					openTabs = new Vector.<ZoomPanCanvas>;
					mapOpenTabs = new Vector.<int>;

				}
			}
			
			if ( !infoPanel ) {
				infoPanel = new Panel();
				infoPanel.title = "Info";
				divider.addChild(infoPanel);
				
				if ( !infoDivider ) { 
					infoDivider = new VDividedBox();
					infoDivider.liveDragging = false;
					infoPanel.addChild(infoDivider);
				
					if ( !infoContent ) {
						infoContent = new TextArea();
						infoContent.setStyle("backgroundAlpha", infoPanel.getStyle("backgroundAlpha"));
						infoDivider.addChild(infoContent);
					}
					
					if ( !infoNavigator ) {
						infoNavigator = new TabNavigator();
						infoDivider.addChild(infoNavigator);
						
						if ( !modulesSearchableDataGrid ) {
							modulesSearchableDataGrid = new SearchableDataGrid();
							modulesSearchableDataGrid.label = "Modules";
							modulesSearchableDataGrid.addEventListener(SearchableDataGridSelectionEvent.ITEM_CLICK, clickModulesHandler);
							modulesSearchableDataGrid.addEventListener(SearchableDataGridSelectionEvent.ITEM_DOUBLE_CLICK, doubleClickModulesHandler);
							infoNavigator.addChild(modulesSearchableDataGrid);
						}
	
						if ( !genesSearchableDataGrid ) {
							genesSearchableDataGrid = new SearchableDataGrid();
							genesSearchableDataGrid.label = "Genes";
							genesSearchableDataGrid.addEventListener(SearchableDataGridSelectionEvent.ITEM_CLICK, clickGenesHandler);
							genesSearchableDataGrid.addEventListener(SearchableDataGridSelectionEvent.ITEM_DOUBLE_CLICK, doubleClickGenesHandler);
							infoNavigator.addChild(genesSearchableDataGrid);
						}			
						
						if ( !samplesSearchableDataGrid ) {
							samplesSearchableDataGrid = new SearchableDataGrid();
							samplesSearchableDataGrid.label = "Samples";
							samplesSearchableDataGrid.addEventListener(SearchableDataGridSelectionEvent.ITEM_CLICK, clickSamplesHandler);
							samplesSearchableDataGrid.addEventListener(SearchableDataGridSelectionEvent.ITEM_DOUBLE_CLICK, doubleClickSamplesHandler);
							infoNavigator.addChild(samplesSearchableDataGrid);
						}
						
						if ( !GOSearchableDataGrid ) {
							GOSearchableDataGrid = new SearchableDataGrid();
							GOSearchableDataGrid.label = "GO";
							infoNavigator.addChild(GOSearchableDataGrid);
						}

						if ( !KEGGSearchableDataGrid ) {
							KEGGSearchableDataGrid = new SearchableDataGrid();
							KEGGSearchableDataGrid.label = "KEGG";
							infoNavigator.addChild(KEGGSearchableDataGrid);
						}

						if ( !experimentData ) {
							experimentData = new Canvas();
							experimentData.label = "Experiment";
							infoNavigator.addChild(experimentData);
							if ( !experimentDataContent ) {
								experimentDataContent = new TextArea();
								experimentData.addChild(experimentDataContent);
							}
						}
						
					}
				}
				
			}
			
			useDefaultPositions = true;
			
			parentApplication.addEventListener(MenuEvent.DEFAULT_POSITIONS, setDefaultPositionsHandler);
			parentApplication.addEventListener(UpdateGEDataEvent.UPDATEGEDATAEVENT, updateGEDataHandler);
			parentApplication.addEventListener(MenuEvent.MODE, modeChangeHandler);
			parentApplication.addEventListener(BroadcastPositionEvent.MOUSE_OVER, broadcastPositionOverHandler);
			parentApplication.addEventListener(BroadcastPositionEvent.MOUSE_CLICK, broadcastPositionClickHandler);
			parentApplication.addEventListener(MenuEvent.PDF_EXPORT, pdfExportHandler);
			parentApplication.addEventListener(MenuEvent.EXCEL_EXPORT, excelExportHandler);
			parentApplication.addEventListener(ResizeBrowserEvent.RESIZEBROWSEREVENT, resizeBrowserHandler);
			parentApplication.addEventListener(MenuEvent.ALPHA, alphaSliderChangeHandler);
		}

		private function alphaSliderChangeHandler(event:MenuEvent): void {
			selectedAlpha = event.data[0];
		}
		
		private function resizeBrowserHandler(event:ResizeBrowserEvent): void {
			
			var scalex:Number = event.scaleX;
			var scaley:Number = event.scaleY;
			
			if ( isFinite(scalex) && isFinite(scaley) ) {
				gePanel.x *= scalex;
				infoPanel.x *= scalex;
				
				gePanel.width *= scalex;
				infoPanel.width *= scalex;

				gePanel.y *= scaley;
				infoPanel.y *= scaley;

				gePanel.height *= scaley;
				infoPanel.height *= scaley;
			}
			
		}
		
		override protected function updateDisplayList(unscaledWidth:Number, unscaledHeight:Number):void {
			
			super.updateDisplayList(unscaledWidth, unscaledHeight);

			divider.percentWidth = 100;
			divider.percentHeight = 100;
			
			gePanel.percentHeight = 100;
			infoPanel.percentHeight = 100;

			modulesNavigator.percentWidth = 100;
			modulesNavigator.percentHeight = 100;							
			for ( var module:int = 0; module < openTabs.length; ++module ) {
				openTabs[module].percentWidth = 100;
				openTabs[module].percentHeight = 100;
			}
			
			infoDivider.percentWidth = 100;
			infoDivider.percentHeight = 100;
			infoContent.percentWidth = 100;
			infoContent.percentHeight = 100;
			infoNavigator.percentWidth = 100;
			infoNavigator.percentHeight = 100;
			modulesSearchableDataGrid.percentWidth = 100;
			modulesSearchableDataGrid.percentHeight = 100;
			genesSearchableDataGrid.percentWidth = 100;
			genesSearchableDataGrid.percentHeight = 100;
			samplesSearchableDataGrid.percentWidth = 100;
			samplesSearchableDataGrid.percentHeight = 100;
			experimentData.percentWidth = 100;
			experimentData.percentHeight = 100;
			experimentDataContent.percentWidth = 100;
			experimentDataContent.percentHeight = 100;
			
			if ( useDefaultPositions ) {
				gePanel.percentWidth = 60;
				infoPanel.percentWidth = 40;
				infoContent.percentHeight = 20;
				infoNavigator.percentHeight = 80;
				if ( infoContent.height > 0 ) {
					useDefaultPositions = false;
				}
			}
			
		}

		private function modeChangeHandler(event:MenuEvent): void {
			var mode:int = event.data[0];
			if ( mode != selectedMode ) {
				lastHighlightedModules = new Array();
			}
			selectedMode = mode;
		}

		private function toggleScoreColumns(state:Boolean):void {
			var temp:Array = genesSearchableDataGrid.columns;
			for ( var i:int = 0; i < temp.length; ++i ) {
				if ( temp[i].dataField == "score" ) {
					temp[i].visible = state;
				}
			}
			genesSearchableDataGrid.columns = temp;
			temp = samplesSearchableDataGrid.columns;
			for ( i = 0; i < temp.length; ++i ) {
				if ( temp[i].dataField == "score" ) {
					temp[i].visible = state;
				}
			}
			samplesSearchableDataGrid.columns = temp;
			scoreColumnsVisible = true;
		}
		
		private function tabChangeHandler(event:IndexChangedEvent): void {

			openTabs[event.newIndex].addListener();
			var module:int = mapOpenTabs[event.newIndex];
			if ( module == 0 ) {
				toggleScoreColumns(false);
				infoNavigator.getTabAt(3).visible = false;
				infoNavigator.getTabAt(4).visible = false;
				infoNavigator.getTabAt(3).includeInLayout = false;
				infoNavigator.getTabAt(4).includeInLayout = false;
			} else {
				toggleScoreColumns(true);
				infoNavigator.getTabAt(3).visible = true;
				infoNavigator.getTabAt(4).visible = true;				
				infoNavigator.getTabAt(3).includeInLayout = true;
				infoNavigator.getTabAt(4).includeInLayout = true;
			} 
			genesSearchableDataGrid.dataProvider = ged.getModule(module).Genes;
			samplesSearchableDataGrid.dataProvider = ged.getModule(module).Samples;
			GOSearchableDataGrid.dataProvider = ged.getModule(module).GO;
			KEGGSearchableDataGrid.dataProvider = ged.getModule(module).KEGG;
			dispatchEvent(new MenuEvent(MenuEvent.MODE, [selectedMode]));
			
			if ( event.oldIndex < modulesNavigator.numChildren ) {
				openTabs[event.oldIndex].removeListener();
				var oldmodule:int = mapOpenTabs[event.oldIndex];
			}			
			
		}
		
		private function tabCloseHandler(event:ClosableTabNavigatorEvent): void {
			var tabIndex:int = event.data[0];
			if ( tabIndex != 0 ) {
				modulesNavigator.selectedIndex = tabIndex - 1;
				modulesNavigator.removeChildAt(tabIndex);			
				openTabs.splice(tabIndex, 1);
				mapOpenTabs.splice(tabIndex, 1);
			}
		}
		
		private function tabReorderHandler(event:IndexChangedEvent): void {
			event.preventDefault();
			Alert.show("Closing tabs is not yet supported.", 'Warning', mx.controls.Alert.OK)
			/*
			trace(event.oldIndex, event.newIndex);
			if ( event.oldIndex == 0 ) {
				event.preventDefault();
			} else {
				var temp:int = mapOpenTabs[event.oldIndex];
				mapOpenTabs[event.oldIndex] = mapOpenTabs[event.newIndex];
				mapOpenTabs[event.newIndex] = temp;
			}
			*/
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
					modulesNavigator.enableClose = [selectedTab, ClosableTab.ROLLOVER];
					openTabs[selectedTab].label = "m" + selectedModule.toString();

					var largestRectangles:Array = new Array(ged.nModules + 1);
					largestRectangles[0] = new Rectangle();
					var maxwidth:int = 0;
					var maxheight:int = 0;
					for ( var module:int = 1; module <= ged.nModules; ++module ) {
						if ( gem.ModulesRectangles[module] != null ) {
							largestRectangles[module] = gem.ModulesRectangles[module][gem.ModulesOutlines[module]];
							for ( var j:int = 0; j < gem.ModulesRectangles[module].length; ++j ) {
								maxwidth = Math.max(maxwidth, gem.ModulesRectangles[module][j].bottomRight.x);
								maxheight = Math.max(maxheight, gem.ModulesRectangles[module][j].bottomRight.y);
							}
						} else {
							largestRectangles[module] = new Rectangle();
						}
					}
					openTabs[selectedTab].dataProvider = new Array(gem.GEImage, gem.ModulesImage, largestRectangles, ged.ModulesColors, [maxwidth, maxheight]);
					genesSearchableDataGrid.dataProvider = gem.Genes;
					samplesSearchableDataGrid.dataProvider = gem.Samples;
					if ( scoreColumnsVisible == false ) {
						toggleScoreColumns(true);
					}
					GOSearchableDataGrid.dataProvider = gem.GO;
					KEGGSearchableDataGrid.dataProvider = gem.KEGG;
					mapOpenTabs.push(selectedModule);
				}
				var lastSelectedTab:int = modulesNavigator.selectedIndex;
				dispatchEvent(new MenuEvent(MenuEvent.ALPHA, [selectedAlpha]));
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

		private function broadcastPositionClickHandler(event:BroadcastPositionEvent): void {
			var gene:int = event.data[0];
			var sample:int = event.data[1];
			var module:int = mapOpenTabs[modulesNavigator.selectedIndex]
			// Array returned is Array(geneDescription, sampleDescription, modules, data, modulesRectangles);
			var infoArray:Array = ged.getInfo(module, gene, sample);
			var modules:Array = infoArray[2];
			if ( modules.length != 0 ) {
				modulesSearchableDataGrid.dispatchEvent(new SearchableDataGridSelectionEvent(SearchableDataGridSelectionEvent.ITEM_DOUBLE_CLICK, modules));
			}
		}

		private function broadcastPositionOverHandler(event:BroadcastPositionEvent): void {
			var gene:int = event.data[0];
			var sample:int = event.data[1];
			
			infoContent.text = "";
			var module:int = mapOpenTabs[modulesNavigator.selectedIndex]
			// Array returned is Array(geneDescription, sampleDescription, modules, data, modulesRectangles);
			var infoArray:Array = ged.getInfo(module, gene, sample);
			if ( infoArray != null ) {
				var modules:Array = infoArray[2];
				// update infoPanel
				var infoString:String = "";
				if ( infoArray.length != 0 ) {
					infoString = infoString + "Gene: " + infoArray[0].symbol + " (" + infoArray[0].name + ")";
					infoString = infoString + "\nSample: " + infoArray[1].name;
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
				
		private function updateGEDataHandler(event:UpdateGEDataEvent): void {
			rawged = event.data[0];
			ged = event.data[1];
			var gem:GeneExpressionModule = ged.getModule(0);

			var title:String = ged.XMLData.experimentdata.title;
			if ( title.length > 80 ) {
				title = title.substr(0,80) + "..." 
			}
			title += ": " + ged.nGenes + " Genes, " + ged.nSamples + " Samples and " + ged.nModules + " Modules";
			gePanel.title = title;
			
			// hide GO and KEGG tabs
			infoNavigator.getTabAt(3).visible = false;
			infoNavigator.getTabAt(4).visible = false;
			infoNavigator.getTabAt(3).includeInLayout = false;
			infoNavigator.getTabAt(4).includeInLayout = false;

			modulesSearchableDataGrid.dataProvider = ged.Modules;
			genesSearchableDataGrid.dataProvider = gem.Genes;
			samplesSearchableDataGrid.dataProvider = gem.Samples;
			// genes
			var wrap:Boolean = false;
			var temp:Array = [];
			for ( var i:int = 0; i < ged.geneLabels.length; i++ ) {
				var column:DataGridColumn = new DataGridColumn();
				column.dataField = ged.geneLabels[i][0];
				column.headerText = ged.geneLabels[i][1];
				column.headerWordWrap = wrap;
				column.headerRenderer = new ClassFactory(Label);
				if ( column.dataField == "score" ) {
					column.visible = false;
				}
				// show hyperlinks
				var linkRenderer:ClassFactory = new ClassFactory(LinkRenderer);
				if ( ged.geneLabels[i][0] == "symbol" || ged.geneLabels[i][0] == "entrezid" ) {
					var database:String = "entrez";
					if ( ged.XMLData.experimentdata.organism == "Homo sapiens" ) {
						database = "genecard";
					}
					linkRenderer.properties = { dataProvider : database }
					column.itemRenderer = linkRenderer;	
				}
				column.sortCompareFunction = sortFunction(column.dataField);
				temp.push(column);
			}
			genesSearchableDataGrid.columns = temp;
			// samples
			temp = [];
			for ( i = 0; i < ged.sampleLabels.length; i++ ) {
				column = new DataGridColumn();
				column.dataField = ged.sampleLabels[i][0];
				column.headerText = ged.sampleLabels[i][1];
				column.headerWordWrap = wrap;
				column.headerRenderer = new ClassFactory(Label);
				if ( column.dataField == "score" ) {
					column.visible = false;
				}
				column.sortCompareFunction = sortFunction(column.dataField);
				temp.push(column)
			}
			samplesSearchableDataGrid.columns = temp;
			// modules
			temp = [];
			for ( i = 0; i < ged.moduleLabels.length; i++ ) {
				column = new DataGridColumn();
				column.dataField = ged.moduleLabels[i][0];
				column.headerText = ged.moduleLabels[i][1];
				column.headerWordWrap = wrap;
				column.headerRenderer = new ClassFactory(Label);
				column.sortCompareFunction = sortFunction(column.dataField);
				temp.push(column)
			}
			modulesSearchableDataGrid.columns = temp;
			// GO
			temp = [];
			for ( i = 0; i < ged.goLabels.length; i++ ) {
				column = new DataGridColumn();
				column.dataField = ged.goLabels[i][0];
				column.headerText = ged.goLabels[i][1];
				column.headerWordWrap = wrap;
				column.headerRenderer = new ClassFactory(Label);
				// show hyperlinks
				linkRenderer = new ClassFactory(LinkRenderer);
				if ( ged.goLabels[i][0] == "go" ) {
					database = "go";
					linkRenderer.properties = { dataProvider : database }
					column.itemRenderer = linkRenderer;	
				}
				column.sortCompareFunction = sortFunction(column.dataField);
				temp.push(column);
			}
			GOSearchableDataGrid.columns = temp;
			// KEGG
			temp = [];
			for ( i = 0; i < ged.keggLabels.length; i++ ) {
				column = new DataGridColumn();
				column.dataField = ged.keggLabels[i][0];
				column.headerText = ged.keggLabels[i][1];
				column.headerWordWrap = wrap;
				column.headerRenderer = new ClassFactory(Label);
				// show hyperlinks
				linkRenderer = new ClassFactory(LinkRenderer);
				if ( ged.keggLabels[i][0] == "kegg" ) {
					database = "kegg";
					linkRenderer.properties = { dataProvider : database }
					column.itemRenderer = linkRenderer;	
				}
				column.sortCompareFunction = sortFunction(column.dataField);
				temp.push(column);
			}
			KEGGSearchableDataGrid.columns = temp;
			generategedatainfo();

			modulesNavigator.removeAllChildren();
			
			openTabs = new Vector.<ZoomPanCanvas>;					
			var selectedTab:int = openTabs.push(new ZoomPanCanvas()) - 1;
			modulesNavigator.addChild(openTabs[selectedTab]);

			modulesNavigator.enableClose = [selectedTab, ClosableTab.NEVER];

			var largestRectangles:Array = new Array(ged.nModules + 1);
			largestRectangles[0] = new Rectangle();
			var maxwidth:int = 0;
			var maxheight:int = 0;
			for ( var module:int = 1; module <= ged.nModules; ++module ) {
				largestRectangles[module] = gem.ModulesRectangles[module][gem.ModulesOutlines[module]];
				for ( var j:int = 0; j < gem.ModulesRectangles[module].length; ++j ) {
					maxwidth = Math.max(maxwidth, gem.ModulesRectangles[module][j].bottomRight.x);
					maxheight = Math.max(maxheight, gem.ModulesRectangles[module][j].bottomRight.y);
				}
			}
			openTabs[selectedTab].label = "Global";
			openTabs[selectedTab].dataProvider = new Array(gem.GEImage, gem.ModulesImage, largestRectangles, ged.ModulesColors, [maxwidth, maxheight]);
			openTabs[selectedTab].addListener();
			openTabs[selectedTab].addEventListener(MouseEvent.ROLL_OUT, rollOutHandler);

			mapOpenTabs = new Vector.<int>;
			mapOpenTabs.push(0);							
		}
		
		private function sortFunction(sortfield:String):Function {
			return function (obj1:Object, obj2:Object):int {
				var result:int;
				if ( isNaN(Number(obj1.child(sortfield))) ) {
					result = ObjectUtil.stringCompare(obj1.child(sortfield),obj2.child(sortfield),true);
				} else {
					result = ObjectUtil.numericCompare(Number(obj1.child(sortfield)),Number(obj2.child(sortfield)));
				}        		
	    	    return result;
    		}
		}
		
		private function rollOutHandler(event:MouseEvent): void {
			dispatchEvent(new HighlightingEvent(HighlightingEvent.MODULE, [[]]))
		}
		
		private function pdfExportHandler(event:MenuEvent): void {
			
			if ( openTabs.length == 0 ) {
				return;
			}
			var module:int = modulesNavigator.selectedIndex;
			var pdfBitmap:Bitmap = openTabs[module].getBitmap();
			
			var myPDF:PDF;
			myPDF = new PDF(Orientation.PORTRAIT, Unit.MM, Size.A4);
			myPDF.addPage();
			myPDF.textStyle(new RGBColor(10));
			myPDF.setFont(FontFamily.HELVETICA, Style.BOLD);
			myPDF.setFontSize(12);
			var title:String = ged.XMLData.experimentdata.title;
			if ( title != "" ) {
				myPDF.setXY(10,10);
				myPDF.addMultiCell(190, 6, title);
			}
			myPDF.addImage(pdfBitmap,10,myPDF.getY() + 10,190,0);

			var bytes:ByteArray = myPDF.save(Method.LOCAL);
			var file:FileReference = new FileReference();
			file.save(bytes);
	
		}
		
		private function excelExportHandler(event:MenuEvent): void {
			
			if ( openTabs.length == 0 ) {
				return;
			}
			
			var bytes:ByteArray = new ByteArray();
			
			var module:int = mapOpenTabs[modulesNavigator.selectedIndex];
			var gem:GeneExpressionModule = ged.getModule(module);
			var genes:XMLListCollection = gem.Genes;
			var samples:XMLListCollection = gem.Samples;
			
			var tag:String = "MODULE " + module.toString() + ": ";
			if ( module == 0 ) { tag = ""; }
			bytes.writeUTFBytes(tag + gem.nSamples + " samples (rows) x " + gem.nGenes + " genes (columns)");
			for ( var gene:int = 0; gene < genes.length; ++gene ) {
				bytes.writeUTFBytes(", " + genes[gene].name);
			}
			bytes.writeUTFBytes("\n"); 
			for ( var sample:int = 0; sample < samples.length; ++sample ) {
				bytes.writeUTFBytes(samples[sample].name + ", ");
				for ( gene = 0; gene < genes.length-1; ++gene ) {
					bytes.writeUTFBytes(ged.getInfo(module, gene, sample)[3].toString() + ", ");
				}
				bytes.writeUTFBytes(ged.getInfo(module, gene, sample)[3].toString() + "\n");
			}
			
			var file:FileReference = new FileReference();
			file.save(bytes);
			
		}
				
		private function generategedatainfo(): void {
			if ( ged.XMLData.experimentdata.title != "" ) {
				experimentDataContent.htmlText = "<b>" + ged.XMLData.experimentdata.title + "</b><br><br>";
			} 
			if ( ged.XMLData.experimentdata.name != "" ) {
				experimentDataContent.htmlText += ged.XMLData.experimentdata.name + "<br><br>";
			} 
			if ( ged.XMLData.experimentdata.lab != "" ) {
				experimentDataContent.htmlText += "<i>" + ged.XMLData.experimentdata.lab + "</i><br><br>";
			} 
			if ( ged.XMLData.experimentdata.abstract != "" ) {
				experimentDataContent.htmlText += "<p>" + ged.XMLData.experimentdata.abstract + "</p><br><br>";
			}
			if ( ged.XMLData.experimentdata.url != "" ) {
				experimentDataContent.htmlText += ged.XMLData.experimentdata.url + "<br>";
			}
			if ( ged.XMLData.experimentdata.annotation != "" ) {
				experimentDataContent.htmlText += "Annotation: " + ged.XMLData.experimentdata.annotation;
			}
		}
		
	}
}