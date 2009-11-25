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
	import mx.containers.HBox;
	import mx.containers.HDividedBox;
	import mx.containers.Panel;
	import mx.containers.TabNavigator;
	import mx.containers.VBox;
	import mx.containers.VDividedBox;
	import mx.controls.Alert;
	import mx.controls.Button;
	import mx.controls.HRule;
	import mx.controls.TextArea;
	import mx.controls.dataGridClasses.DataGridColumn;
	import mx.core.ClassFactory;
	import mx.core.mx_internal;
	import mx.events.IndexChangedEvent;
	import mx.managers.PopUpManager;
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
		
		private var ged:GeneExpressionData;
		
		private var selectedMode:int;
		private var selectedAlpha:Number = 0.4;
		private var selectedHighlighting:Boolean = true;
		private var selectedOutline:Boolean = true;
		private var selectedFilling:Boolean = true;
		
		private const LABELS:Array = ["Genes", "Samples", "Modules", "GOs", "KEGGs"];
		private var selections:Vector.<Array>;
		private var intersections:Vector.<Array>;
		
		private var lastHighlightedModules:Array;
		private var gos:String;
		private var keggs:String;
		
		private var divider:HDividedBox;
		
		private var gePanel:Panel;
		private var modulesNavigator:ClosableTabNavigator;
		private var openTabs:Vector.<ZoomPanCanvas>;
		private var mapOpenTabs:Vector.<int>;
		
		private var infoPanel:Panel;
		private var infoDivider:VDividedBox;
		private var infoContent:VBox;
		private var infoTextContent:Array;
		private var infoNavigator:TabNavigator;
		private var openButtons:Array;
		private var clearButtons:Array;
		private var infoContentBoxes:Array;
		private var ruler1:HRule;
		private var ruler2:HRule;
		
		private var modulesSearchableDataGrid:SearchableDataGrid;
		private var genesSearchableDataGrid:SearchableDataGrid;		
		private var samplesSearchableDataGrid:SearchableDataGrid;
		private var GOSearchableDataGrid:SearchableDataGrid;
		private var KEGGSearchableDataGrid:SearchableDataGrid;
		private var experimentData:Canvas;
		private var experimentDataContent:TextArea
		 
		private var popup:Panel;

		public function MainCanvas() {
			super();

			ged = new GeneExpressionData();
			lastHighlightedModules = [];
			useDefaultPositions = true;

			selections = new Vector.<Array>(5, true);
			intersections = new Vector.<Array>(5, true);
			for ( var i:int = 0; i < LABELS.length; ++i ) {
				selections[i] = [];
				intersections[i] = [];
			}
			
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
						infoContent = new VBox();
						infoContent.setStyle("backgroundAlpha", infoPanel.getStyle("backgroundAlpha"));
						infoContent.setStyle("backgroundColor", "#ffffff");
						infoContent.setStyle("verticalGap", "0");
						infoContent.verticalScrollPolicy = "off";
						infoContent.styleName = "infoContent";
						infoDivider.addChild(infoContent);

						if ( !ruler1 ) {
							ruler1 = new HRule();
							ruler1.visible = false;
						}
						if ( !ruler2 ) {
							ruler2 = new HRule();
							ruler2.visible = false;
						}

						infoContentBoxes = [];
						for ( var i:int = 0; i < 7; ++i ) {
							var hbox:HBox = new HBox();
							hbox.visible = false;
							hbox.setStyle("verticalAlign", "middle");
							infoContentBoxes.push(hbox);
						}

						for ( i = 0; i < 2; ++i ) {
							infoContent.addChild(infoContentBoxes[i]);
						}
						infoContent.addChild(ruler1);
						for ( i = 2; i < 5; ++i ) {
							infoContent.addChild(infoContentBoxes[i]);
						}
						infoContent.addChild(ruler2);
						for ( i = 5; i < 7; ++i ) {
							infoContent.addChild(infoContentBoxes[i]);
						}

						infoTextContent = [];
						for ( i = 0; i < infoContentBoxes.length; ++i ) {
							var textContent:TextArea = new TextArea();
							textContent.editable = false;
							textContent.selectable = true;
							textContent.verticalScrollPolicy = "off";
							textContent.styleName = "infoTextContent";
							infoTextContent.push(textContent);
							infoContentBoxes[i].addChild(infoTextContent[i]);
						}

						var LABELS:Array = ["Genes", "Samples", "Modules", "GO", "KEGG"];

						openButtons = [];
						for ( i = 0; i < 7; ++i ) {
							var button:Button = new Button();
							button.styleName = "openModuleButton";
							button.name = "openButton" + i;
							button.toolTip = "Open intersecting modules";
							button.addEventListener(MouseEvent.CLICK, openModuleButtonClickHandler);
							openButtons.push(button);
						}						
						
						clearButtons = [];
						for ( i = 0; i < 5; ++i ) {
							button = new Button();
							button.styleName = "clearButton";
							button.name = "clearButton" + i;
							button.toolTip = "Clear selected " + LABELS[i];
							button.addEventListener(MouseEvent.CLICK, clearButtonClickHandler);
							clearButtons.push(button);
							infoContentBoxes[i+2].addChild(openButtons[i]);
							infoContentBoxes[i+2].addChild(button);
							infoContentBoxes[i+2].addChild(infoTextContent[i+2])
						}						

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
							GOSearchableDataGrid.addEventListener(SearchableDataGridSelectionEvent.ITEM_CLICK, clickGOHandler);
							infoNavigator.addChild(GOSearchableDataGrid);
						}

						if ( !KEGGSearchableDataGrid ) {
							KEGGSearchableDataGrid = new SearchableDataGrid();
							KEGGSearchableDataGrid.label = "KEGG";
							KEGGSearchableDataGrid.addEventListener(SearchableDataGridSelectionEvent.ITEM_CLICK, clickKEGGHandler);
							infoNavigator.addChild(KEGGSearchableDataGrid);
						}

						if ( !experimentData ) {
							experimentData = new Canvas();
							experimentData.label = "Experiment";
							infoNavigator.addChild(experimentData);
							if ( !experimentDataContent ) {
								experimentDataContent = new TextArea();
								experimentDataContent.editable = false;
								experimentDataContent.selectable = true;
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
			parentApplication.addEventListener(MenuEvent.HIGHLIGHTING, highlightingChangeHandler);
			parentApplication.addEventListener(MenuEvent.FILLING, fillingChangeHandler);
			parentApplication.addEventListener(MenuEvent.OUTLINE, outlineChangeHandler);
		}

		private function openModuleButtonClickHandler(event:MouseEvent): void {
			var temp:String = event.target.name;
			var i:int = int(temp.slice(-1, temp.length));
			modulesSearchableDataGrid.dispatchEvent(new SearchableDataGridSelectionEvent(SearchableDataGridSelectionEvent.ITEM_DOUBLE_CLICK, intersections[i]));			
		}

		private function clearButtonClickHandler(event:MouseEvent): void {
			var module:int = mapOpenTabs[modulesNavigator.selectedIndex];
			if ( event.target.name == "clearButton0" ) {
				selections[0] = [];
				genesSearchableDataGrid.selectedIndices = [];
				infoTextContent[2].text = "";
			} else if ( event.target.name == "clearButton1" ) {
				selections[1] = [];
				samplesSearchableDataGrid.selectedIndices = [];
				infoTextContent[3].text = "";
			} else if ( event.target.name == "clearButton2" ) {
				selections[2] = [];
				modulesSearchableDataGrid.selectedIndices = [];
				infoTextContent[4].text = "";
			} else if ( event.target.name == "clearButton3" ) {
				selections[3] = [];
				GOSearchableDataGrid.selectedIndices = [];
				infoTextContent[5].text = "";
			} else if ( event.target.name == "clearButton4" ) {
				selections[4] = [];
				KEGGSearchableDataGrid.selectedIndices = [];
				infoTextContent[6].text = "";
			}
			updateHighlighting(module);
		}
	
		private function alphaSliderChangeHandler(event:MenuEvent): void {
			selectedAlpha = event.data[0];
		}
		
		private function highlightingChangeHandler(event:MenuEvent): void {
			selectedHighlighting = event.data[0];
		}

		private function fillingChangeHandler(event:MenuEvent): void {
			selectedFilling = event.data[0];
		}

		private function outlineChangeHandler(event:MenuEvent): void {
			selectedOutline = event.data[0];
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
			for (var i:int = 0; i < infoContentBoxes.length; ++i ) {
				infoContentBoxes[i].percentWidth = 100;
				infoTextContent[i].percentWidth = 100;
			}
			ruler1.percentWidth = 100;
			ruler2.percentWidth = 100;
			
			infoNavigator.percentWidth = 100;
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
		
		private function tabChangeHandler(event:IndexChangedEvent):void {
			openTabs[event.newIndex].addListener();
			var module:int = mapOpenTabs[event.newIndex];
			genesSearchableDataGrid.dataProvider = ged.getModule(module).Genes;
			samplesSearchableDataGrid.dataProvider = ged.getModule(module).Samples;
			GOSearchableDataGrid.dataProvider = ged.getModule(module).GO;
			KEGGSearchableDataGrid.dataProvider = ged.getModule(module).KEGG;
			dispatchEvent(new MenuEvent(MenuEvent.MODE, [selectedMode]));
			
			if ( module == 0 ) {
				toggleScoreColumns( false );
			} else {
				toggleScoreColumns( true );
			}
			updateHighlighting(module);
		}
				
		private function updateHighlighting(module:int): void {			
			var genes:Array = [];
			for ( var gene:int = 0; gene < selections[0].length; ++gene ) {
				var genep:int = (module==0) ? selections[0][gene] : ged.GenesLookupP[module][selections[0][gene].toString()];
				if ( genep > 0 ) {
					genes.push(genep);
				}
			}
			genesSearchableDataGrid.selectedIndices = genes;
			var rectangles:Array = getHRectangles(genes);
			dispatchEvent(new HighlightingEvent(HighlightingEvent.GENE, [rectangles]));
			
			var samples:Array = [];
			for ( var sample:int = 0; sample < selections[1].length; ++sample ) {
				var samplep:int = (module==0) ? selections[1][sample] : ged.SamplesLookupP[module][selections[1][sample].toString()];
				if ( samplep > 0 ) {
					samples.push(samplep);
				}
			}
			samplesSearchableDataGrid.selectedIndices = samples;
			rectangles = getVRectangles(samples);
			dispatchEvent(new HighlightingEvent(HighlightingEvent.SAMPLE, [rectangles]));
			
			modulesSearchableDataGrid.selectedIndices = selections[2];
			var highlightedRectangles:Array = new Array(ged.nModules + 1);
			for ( var i:int = 0; i < selections[2].length; ++i ) {
				var modulep:int = selections[2][i];
				highlightedRectangles[modulep] = ged.getModule(module).ModulesRectangles[modulep];
			}
			dispatchEvent(new HighlightingEvent(HighlightingEvent.MODULE, [highlightedRectangles]));
			
			var gos:Array = [];
			for ( var go:int = 0; go < selections[3].length; ++go ) {
				var gop:int = (module==0) ? selections[3][go] : ged.ModulesLookupGOsP[module][selections[3][go].toString()];
				if ( gop > 0 ) {
					gos.push(gop);
				}
			}
			GOSearchableDataGrid.selectedIndices = gos;
			
			var keggs:Array = [];
			for ( var kegg:int = 0; kegg < selections[4].length; ++kegg ) {
				var keggp:int = (module==0) ? selections[4][kegg] : ged.ModulesLookupKEGGsP[module][selections[4][kegg].toString()];
				if ( keggp > 0 ) {
					keggs.push(keggp);
				}
			}
			KEGGSearchableDataGrid.selectedIndices = keggs;

			invalidateInfoText();
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
			Alert.show("Reordering tabs is not yet supported.", 'Warning', mx.controls.Alert.OK)
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
			selections[2] = [];
			intersections[2] = [];
			for ( var i:int = 0; i < event.selection.length; ++i ) {
				var modulep:int = event.selection[i];
				selections[2].push(modulep);
				highlightedRectangles[modulep] = ged.getModule(module).ModulesRectangles[modulep];
				intersections[2] = intersections[2].concat(ged.ModulesLookupModules[modulep]);
			}
			intersections[2] = removeDuplicates(intersections[2]);
			var string:String = "<b>Selected Modules</b>: " + selections[2].join(", ");;
			if ( intersections[2].length > 0 ) {
				string += "\n<b>Intersecting Modules</b>: " +  intersections[2].join(", ");
			}
			infoTextContent[4].htmlText = string;
			invalidateInfoText();
			dispatchEvent(new HighlightingEvent(HighlightingEvent.MODULE, [highlightedRectangles]));
		}
				
		private function doubleClickModulesHandler(event:SearchableDataGridSelectionEvent): void {
			
			var selection:Array = event.selection;
			
			if (selection.length > 2 ) { 
				popup = new SelectModules(selection);
				popup.title = "select modules to open...";
				popup.addEventListener(PopUpEvent.CANCEL, selectModulesCancelHandler);
				popup.addEventListener(PopUpEvent.OK, selectModulesOkHandler);
        		mx.managers.PopUpManager.addPopUp(popup, gePanel, true);        		
        		mx.managers.PopUpManager.centerPopUp(popup);
			} else {
				doubleClickModulesHandlerP(selection);
			}
		}
		
		private function selectModulesCancelHandler(event:PopUpEvent): void {
			popup.removeEventListener(PopUpEvent.CANCEL, selectModulesCancelHandler);
			popup.removeEventListener(PopUpEvent.OK, selectModulesOkHandler);
			mx.managers.PopUpManager.removePopUp(popup);
		}
		
		private function selectModulesOkHandler(event:PopUpEvent): void {
			popup.removeEventListener(PopUpEvent.CANCEL, selectModulesCancelHandler);
			popup.removeEventListener(PopUpEvent.OK, selectModulesOkHandler);
			mx.managers.PopUpManager.removePopUp(popup);
			var selection:Array = event.data[0];
			doubleClickModulesHandlerP(selection);
		}
		
		private function doubleClickModulesHandlerP(selection:Array):void {
			
			for ( var i:int = 0; i < selection.length; ++i ) {
				var selectedModule:int = selection[i];
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
					openTabs[selectedTab].dataProvider = new Array(gem.GEImage, gem.ModulesImage, largestRectangles, ged.ModulesColors, [maxwidth, maxheight], true);
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
				dispatchEvent(new MenuEvent(MenuEvent.HIGHLIGHTING, [selectedHighlighting]));
				dispatchEvent(new MenuEvent(MenuEvent.FILLING, [selectedFilling]));
				dispatchEvent(new MenuEvent(MenuEvent.OUTLINE, [selectedOutline]));
				if ( lastSelectedTab != selectedTab ) { 
					openTabs[lastSelectedTab].removeListener();
					openTabs[selectedTab].addListener();
					modulesNavigator.selectedIndex = selectedTab;
				}
				
				updateHighlighting(selectedModule);
				
			}			
		}
		
		private function getHRectangles(slots:Array):Array {
			slots.sort(Array.NUMERIC);
			var rectxleft:Array = []; var rectxright:Array = [];
			var oldslot:int = slots[0];
			rectxleft.push(oldslot);
			for ( var slotp:int = 0; slotp < slots.length; ++slotp ) {
				var slot:int = slots[slotp];
				if ( slot > oldslot + 1 ) {
					rectxright.push(oldslot);
					rectxleft.push(slot);
				}
				oldslot = slot;
			};
			rectxright.push(oldslot);
			
			var rectangles:Array = [];
			for ( var i:int = 0; i < rectxleft.length; ++i ) {
				var x:Number = rectxleft[i] - 1;
				var y:Number = 0;
				var dx:Number = rectxright[i] - x;
				var dy:Number = ged.nSamples;
				rectangles.push(new Rectangle(x, y, dx, dy));  
			}
			return(rectangles);
		}
		
		private function getVRectangles(slots:Array):Array {
			slots.sort(Array.NUMERIC);
			var rectytop:Array = []; var rectybottom:Array = [];				
			var oldslot:int = slots[0];
			rectytop.push(oldslot);
			for ( var slotp:int = 0; slotp < slots.length; ++slotp ) {
				var slot:int = slots[slotp];
				if ( slot > oldslot + 1 ) {
					rectybottom.push(oldslot);
					rectytop.push(slot);
				}
				oldslot = slot;
			};
			rectybottom.push(oldslot);

			var rectangles:Array = [];
			for ( var i:int = 0; i < rectytop.length; ++i ) {
				var x:Number = 0;
				var y:Number = rectytop[i] - 1;
				var dx:Number = ged.nGenes;
				var dy:Number = rectybottom[i] - y; 
				rectangles.push(new Rectangle(x, y, dx, dy));  
			}
			return rectangles;
		}
		
		private function removeDuplicates(data:Array):Array {
			if ( data.length == 0 ) {
				return [];
			}
			data.sort(Array.NUMERIC);
			var temp:Array = [data[0]];
			for ( var i:int = 1; i < data.length; ++i ) {
				if ( data[i] != data[i-1] ) {
					temp.push(data[i]);
				}
			}		
			return temp;
		}

		private function invalidateInfoText(): void {
			var first:Boolean = false;
			for ( var i:int = 0; i < infoTextContent.length; ++i ) {
				infoTextContent[i].validateNow();
				var temp:Number = 0;
				for( var j:int=0; j < infoTextContent[i].mx_internal::getTextField().numLines; ++j) {
					temp += infoTextContent[i].mx_internal::getTextField().getLineMetrics(j).height;
				}
				if ( infoTextContent[i].text == "" ) {
					infoTextContent[i].height = 0;
					infoContentBoxes[i].height = 0;
					infoContentBoxes[i].visible = false;
				} else {
					infoContentBoxes[i].visible = true;
					infoTextContent[i].height = temp + 5;
					infoContentBoxes[i].height = temp + 5;
				}
			}
			var showruler:Boolean = false;
			for ( i = 2; i < 5; ++i ) {
				if ( infoContentBoxes[i].visible ) {
					showruler = true;
				}
			}
			ruler1.visible = ( showruler ) ? true : false;
			showruler = false;
			for ( i = 5; i < 7; ++i ) {
				if ( infoContentBoxes[i].visible ) {
					showruler = true;
				}
			}
			ruler2.visible = ( showruler ) ? true : false;
		}

		private function clickGenesHandler(event:SearchableDataGridSelectionEvent): void {
			var module:int = mapOpenTabs[modulesNavigator.selectedIndex];
			var genes:Array = event.selection;
			genes.sort(Array.NUMERIC);
			selections[0] = [];
			intersections[0] = [];
			for ( var gene:int = 0; gene < genes.length; ++gene ) {
				var genep:int = genes[gene];
				selections[0].push( (module==0) ? genep : ged.GenesLookup[module][genep-1]);
				intersections[0] = intersections[0].concat(ged.ModulesLookupGenes[genep]);
			}
			intersections[0] = removeDuplicates(intersections[0]);
			var rectangles:Array = getHRectangles(genes);
			dispatchEvent(new HighlightingEvent(HighlightingEvent.GENE, [rectangles]));
			var genesp:Array = [];
			for ( gene = 0; gene < genes.length; ++gene ) {
				genesp.push(ged.getModule(module).Genes.source[genes[gene]-1].symbol.text());
			}
			var string:String = "<b>Selected Genes</b>: " +  genesp.join(", ");
			if ( intersections[0].length > 0 ) {
				string += "\n<b>Intersecting Modules</b>: " +  intersections[0].join(", ");
			}
			infoTextContent[2].htmlText = string;
			invalidateInfoText();
		}
		private function doubleClickGenesHandler(event:SearchableDataGridSelectionEvent): void {
		}

		private function clickSamplesHandler(event:SearchableDataGridSelectionEvent): void {
			var module:int = mapOpenTabs[modulesNavigator.selectedIndex];
			var samples:Array = event.selection;
			samples.sort(Array.NUMERIC);
			selections[1] = [];
			intersections[1] = [];
			for ( var sample:int = 0; sample < samples.length; ++sample ) {
				var samplep:int = samples[sample];
				selections[1].push( (module==0) ? samplep: ged.SamplesLookup[module][samplep-1]);
				intersections[1] = intersections[1].concat(ged.ModulesLookupSamples[samplep]);
			}
			intersections[1] = removeDuplicates(intersections[1]);
			var rectangles:Array = getVRectangles(samples);
			dispatchEvent(new HighlightingEvent(HighlightingEvent.SAMPLE, [rectangles]));
			var samplesp:Array = [];
			for ( sample = 0; sample < samples.length; ++sample ) {
				samplesp.push(ged.getModule(module).Samples.source[samples[sample]-1].name.text());
			}
			var string:String = "<b>Selected Samples</b>: " +  samplesp.join(", ");
			if ( intersections[1].length > 0 ) {
				string += "\n<b>Intersecting Modules</b>: " +  intersections[1].join(", ");
			}
			infoTextContent[3].htmlText = string;
			invalidateInfoText();
		}
		private function doubleClickSamplesHandler(event:SearchableDataGridSelectionEvent): void {
		}

		private function clickGOHandler(event:SearchableDataGridSelectionEvent): void {
			var module:int = mapOpenTabs[modulesNavigator.selectedIndex];
			var GOs:Array = event.selection;
			GOs.sort(Array.NUMERIC);
			selections[3] = [];
			intersections[3] = [];
			var gos:Array = [];
			for ( var i:int = 0; i < GOs.length; ++i ) {
				var j:int = (module==0) ? GOs[i] : ged.ModulesLookupGOs[module][GOs[i]];
				selections[3].push(j);
				gos.push(ged.getModule(module).GO.source[GOs[i]-1].term);
				intersections[3].push(ged.ModulesLookupGOs[0][j-1]);
			}
			intersections[3] = removeDuplicates(intersections[3]);
			var string:String = "<b>Selected GOs</b>: " +  gos.join(", ");
			if ( intersections[3].length > 0 ) {
				string += "\n<b>Found in Modules</b>: " +  intersections[3].join(", ");
			}
			infoTextContent[5].htmlText = string;
			invalidateInfoText();
		}

		private function clickKEGGHandler(event:SearchableDataGridSelectionEvent): void {
			var module:int = mapOpenTabs[modulesNavigator.selectedIndex];
			var KEGGs:Array = event.selection;
			KEGGs.sort(Array.NUMERIC);
			selections[4] = [];
			intersections[4] = [];
			var keggs:Array = [];
			for ( var i:int = 0; i < KEGGs.length; ++i ) {
				var j:int = (module==0) ? KEGGs[i] : ged.ModulesLookupKEGGs[module][KEGGs[i]];
				selections[4].push(j);
				keggs.push(ged.getModule(module).KEGG.source[KEGGs[i]-1].pathname);
				intersections[4].push(ged.ModulesLookupKEGGs[0][j-1]);
			}
			intersections[4] = removeDuplicates(intersections[4]);
			var string:String = "<b>Selected KEGGs</b>: " +  keggs.join(", ");
			if ( intersections[4].length > 0 ) {
				string += "\n<b>Found in Modules</b>: " +  intersections[4].join(", ");
			}
			infoTextContent[6].htmlText = string;
			invalidateInfoText();
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
			
			var module:int = mapOpenTabs[modulesNavigator.selectedIndex]
			// Array returned is Array(geneDescription, sampleDescription, modules, data, modulesRectangles);
			var infoArray:Array = ged.getInfo(module, gene, sample);
			if ( infoArray != null ) {
				var modules:Array = infoArray[2];
				// update infoPanel
				var infoString:String = "";
				if ( infoArray.length != 0 ) {
					var temp:String = infoArray[0].symbol;
					if ( temp == "" ) {
						temp = infoArray[0].description;
					}
					infoString += "<b>Gene</b>: " + temp + " (" + infoArray[0].name + ")";
					infoString += "\n<b>Sample</b>: " + infoArray[1].name
					infoString += "\n<b>Data</b>: " + infoArray[3]
					infoTextContent[0].htmlText = infoString;
					invalidateInfoText();
					
					infoString = "";
					if ( modules.length > 0 ) {
						infoString = "<b>Modules</b>: " + modules.join(", ");

						if ( modules.length > 0 && module == 0 ) {
								
							// recalculate gos and keggs
							if ( lastHighlightedModules != modules ) {

								var tempsort:Array = [];
								for ( var i:int = 0; i < modules.length; ++i ) {
									var modulep:int = modules[i];
									for ( var j:int = 0; j < ged.XMLData.modules.module[modulep-1].gos.go.length(); ++j ) {
										var item:XML = new XML(ged.XMLData.modules.module[modulep-1].gos.go[j]);
										tempsort.push(new Object());
										tempsort[tempsort.length-1].pvalue = item.pvalue;
										tempsort[tempsort.length-1].name = item.term;
									}
								}
								tempsort.sortOn("pvalue", Array.NUMERIC);
								var gosp:Array = [];
								for ( i = 0; i < tempsort.length; ++i ) {
									var duplicate:Boolean = false;
									var name:String = tempsort[i].name
									for ( j = 0; j < gosp.length; ++j ) {
										if ( name == gosp[j] ) {
											duplicate = true;
											break;
										}
									}
									if ( !duplicate ) {
										gosp.push(name);
									}
									if ( gosp.length == 5 ) {
										break;
									}
								}
								gos = gosp.join(", ");
								
								tempsort = [];
								for ( i = 0; i < modules.length; ++i ) {
									modulep = modules[i];
									for ( j = 0; j < ged.XMLData.modules.module[modulep-1].keggs.kegg.length(); ++j ) {
										item = new XML(ged.XMLData.modules.module[modulep-1].keggs.kegg[j]);
										tempsort.push(new Object());
										tempsort[tempsort.length-1].pvalue = item.pvalue;
										tempsort[tempsort.length-1].name = item.pathname;
									}
								}
								tempsort.sortOn("pvalue", Array.NUMERIC);
								var keggsp:Array = [];
								for ( i = 0; i < tempsort.length; ++i ) {
									duplicate = false;
									name = tempsort[i].name
									for ( j = 0; j < keggsp.length; ++j ) {
										if ( name == keggsp[j] ) {
											duplicate = true;
											break;
										}
									}
									if ( !duplicate ) {
										keggsp.push(name);
									}
									if ( keggsp.length == 5 ) {
										break;
									}
								}
								keggs = keggsp.join(", ");
							}
							
							if ( gos != "" ) {
								infoString += "\n<b>GO</b>: " + gos;
							}
							if ( keggs != "" ) {
								infoString += "\n<b>KEGG</b>: " + keggs;
							}
						}
					}
					infoTextContent[1].htmlText = infoString;
					invalidateInfoText();
					
				}
	
				// highlight module
				modules = modules.concat(selections[2]);
				removeDuplicates(modules);
				if ( selectedHighlighting ) {
					if ( lastHighlightedModules != modules ) {
						var highlightedRectangles:Array = new Array(ged.nModules + 1);
						for ( modulep = 0; modulep < modules.length; ++modulep ) {
							if ( modules[modulep] != module ) {
								highlightedRectangles[modules[modulep]] = ged.getModule(module).ModulesRectangles[modules[modulep]];
							}
						}
						dispatchEvent(new HighlightingEvent(HighlightingEvent.MODULE, [highlightedRectangles]));
						lastHighlightedModules = modules;
					}
				}
			
			} else {
				highlightedRectangles = new Array(ged.nModules + 1);
				for ( i = 0; i < selections[2].length; ++i ) {
					modulep = selections[2][i];
					highlightedRectangles[modulep] = ged.getModule(module).ModulesRectangles[modulep];
				}
				dispatchEvent(new HighlightingEvent(HighlightingEvent.MODULE, [highlightedRectangles]));
			}
		}
		
		private function setDefaultPositionsHandler(event:MenuEvent): void {
			useDefaultPositions = true;
			invalidateDisplayList();
		}
				
		private function updateGEDataHandler(event:UpdateGEDataEvent): void {
			ged = event.data[0];
			var gem:GeneExpressionModule = ged.getModule(0);

			var title:String = ged.XMLData.experimentdata.title;
			if ( title.length > 80 ) {
				title = title.substr(0,80) + "..." 
			}
			title += ": " + ged.nGenes + " Genes, " + ged.nSamples + " Samples and " + ged.nModules + " Modules";
			gePanel.title = title;
			
			modulesSearchableDataGrid.dataProvider = ged.Modules;
			genesSearchableDataGrid.dataProvider = gem.Genes;
			samplesSearchableDataGrid.dataProvider = gem.Samples;
			GOSearchableDataGrid.dataProvider = gem.GO;
			KEGGSearchableDataGrid.dataProvider = gem.KEGG;
						
			// genes
			var wrap:Boolean = false;
			var temp:Array = [];
			for ( var i:int = 0; i < ged.geneLabels.length; i++ ) {
				var column:DataGridColumn = new DataGridColumn();
				column.dataField = ged.geneLabels[i][0];
				column.headerText = ged.geneLabels[i][1];
				column.headerWordWrap = wrap;
				column.headerRenderer = new ClassFactory(HeaderRenderer);
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
				column.headerRenderer = new ClassFactory(HeaderRenderer);
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
				column.headerRenderer = new ClassFactory(HeaderRenderer);
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
				column.headerRenderer = new ClassFactory(HeaderRenderer);
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
				column.headerRenderer = new ClassFactory(HeaderRenderer);
				// show hyperlinks
				linkRenderer = new ClassFactory(LinkRenderer);
				if ( ged.keggLabels[i][0] == "kegg" ) {
					database = "kegg";
					linkRenderer.properties = { dataProvider : database, organism : ged.XMLData.experimentdata.organism }
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
			//dispatchEvent(new HighlightingEvent(HighlightingEvent.MODULE, [[]]))
		}
		
		private function pdfExportHandler(event:MenuEvent): void {
			
			if ( openTabs.length == 0 ) {
				return;
			}
			var module:int = modulesNavigator.selectedIndex;
			var gem:GeneExpressionModule = ged.getModule(module);
			var pdfBitmap:Bitmap = openTabs[module].getBitmap();
			var pdfRectangle:Rectangle = openTabs[module].getRectangle();
			
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
			var y:Number = myPDF.getY();
			myPDF.addImage(pdfBitmap, 10, y + 10, 190, 0);
			var dx:Number = 190;
			var dy:Number = pdfBitmap.height * 190 / pdfBitmap.width;

			myPDF.setFont(FontFamily.HELVETICA, Style.NORMAL);
			myPDF.setFontSize(10);
			for ( var sample:int = pdfRectangle.y; sample < pdfRectangle.bottomRight.y; ++sample ) {
				//myPDF.addText(gem.Samples[sample].name, 10, y + 10 + sample * dy/gem.nSamples);				
			}
			for ( var gene:int = pdfRectangle.x; gene < pdfRectangle.bottomRight.x; ++gene ) {
				//myPDF.addVerticalText(gem.Genes[gene].name, 10 + gene * dx/gem.nGenes, y+dy);				
			}
			
			
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