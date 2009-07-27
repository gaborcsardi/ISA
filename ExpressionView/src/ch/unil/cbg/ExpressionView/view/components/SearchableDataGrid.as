package ch.unil.cbg.ExpressionView.view.components {
	
	import ch.unil.cbg.ExpressionView.events.SearchableDataGridSelectionEvent;
	
	import flash.events.Event;
	import flash.events.IEventDispatcher;
	
	import mx.collections.XMLListCollection;
	import mx.containers.Canvas;
	import mx.containers.HBox;
	import mx.controls.DataGrid;
	import mx.controls.Text;
	import mx.controls.TextInput;
	import mx.controls.dataGridClasses.DataGridColumn;
	import mx.events.ListEvent;

	//[Event(name='ITEM_CLICK', type='ListEvent')]
	//[Event(name='ITEM_DOUBLE_CLICK', type='ListEvent')]
	[Event(name='ITEM_CLICK', type='ch.unil.cbg.ExpressionView.events.SearchableDataGridSelectionEvent')]
	[Event(name='ITEM_DOUBLE_CLICK', type='ch.unil.cbg.ExpressionView.events.SearchableDataGridSelectionEvent')]
	public class SearchableDataGrid extends Canvas implements IEventDispatcher {

		private var dataprovider:XMLListCollection = new XMLListCollection();
		private var datagridcolumns:DataGridColumn = new DataGridColumn();
		private var dataGrid:DataGrid;
		
		private var headerBox:HBox;
		protected var searchField:TextInput;
		private var searchText:Text;
		
		public function SearchableDataGrid() {
			super();
		}

		override protected function createChildren() : void{
			
			super.createChildren();
			super.horizontalScrollPolicy = "off";
			super.verticalScrollPolicy = "off";

			if ( !headerBox ) {
				headerBox = new HBox();
				headerBox.setStyle("verticalAlign", "middle");
				addChild(headerBox);
					
				if ( !searchText ) {
					searchText = new Text();
					searchText.text = "Find";
					headerBox.addChild(searchText);
				}

				if ( !searchField ) {
					searchField = new TextInput();
					searchField.addEventListener(Event.CHANGE, handleChange);
					headerBox.addChild(searchField);
				}
			}
				
			if ( !dataGrid ) {
				dataGrid = new DataGrid();
				dataGrid.allowMultipleSelection = true;
				dataGrid.doubleClickEnabled = true;
				dataGrid.addEventListener(ListEvent.ITEM_CLICK, clickHandler);
				dataGrid.addEventListener(ListEvent.ITEM_DOUBLE_CLICK, doubleClickHandler);
				addChild(dataGrid);
			}
			
		}
		
		private function clickHandler(event:ListEvent): void {			
		}
		
		private function doubleClickHandler(event:ListEvent): void {
			var selected:Array = dataGrid.selectedIndices;
			var selection:Array = [];
			for ( var i:int = 0; i < selected.length; ++ i ) {
				selection.push(dataprovider[selected[i]].children()[0])
			}
			dispatchEvent(new SearchableDataGridSelectionEvent(SearchableDataGridSelectionEvent.ITEM_DOUBLE_CLICK, selection));
		}
		
		override protected function updateDisplayList(unscaledWidth:Number, unscaledHeight:Number):void {		
			super.updateDisplayList(unscaledWidth, unscaledHeight);
			
			searchField.width = parent.width - searchText.width;
			
			headerBox.x = 0; 
			headerBox.y = 0;
			headerBox.percentWidth = 100;
			headerBox.height = 30;
			
			dataGrid.x = 0;
			dataGrid.y = headerBox.height;
			dataGrid.percentWidth = 100;
			dataGrid.height = parent.height - headerBox.height;
			
		}


		private function handleChange(e:Event) : void{
            if (searchField.text.length == 0) {
                dataprovider.filterFunction = null;
            } else {
                dataprovider.filterFunction = filterFunction;
            }
            dataprovider.refresh();			
		}
		
		
		private function filterFunction(item:Object) : Boolean {
            if ( searchField.text.length == 0 ) {
            	return true;
            }
            var f:String = "ig";
            var i:int = 0;
            var regExp:RegExp = new RegExp(searchField.text, f);
            for ( i; i < dataGrid.columns.length; ++i ) {
				var column:String = dataGrid.columns[i].dataField;
				var match:Boolean = regExp.test(item.descendants(column))
				if ( match ) {
					return true;
				}
            }
            return false;
		}
		
		public function set dataProvider(value:Object):void{
			dataprovider = value as XMLListCollection;
			dataGrid.dataProvider = dataprovider;
		}
		
		public function set columns(value:Object):void{
			dataGrid.columns = value as Array;
		}

	}
}