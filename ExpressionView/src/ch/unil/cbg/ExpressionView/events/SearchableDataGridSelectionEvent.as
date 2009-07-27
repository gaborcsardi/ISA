package ch.unil.cbg.ExpressionView.events {
	
	import flash.events.Event;
	
	public class SearchableDataGridSelectionEvent extends Event {

		// file data
		public var selection:Array;		
		public static const ITEM_CLICK:String = "ItemClick";
		public static const ITEM_DOUBLE_CLICK:String = "ItemDoubleClick";
		
		public function SearchableDataGridSelectionEvent(_type:String, _selection:Array, _bubbles:Boolean=true, _cancelable:Boolean=true) {
			super(_type, _bubbles, _cancelable);
			selection = _selection;
		}
		
		override public function clone(): Event {
			return new SearchableDataGridSelectionEvent(type, selection, bubbles, cancelable);
		}

	}
}