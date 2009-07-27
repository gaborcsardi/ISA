package ch.unil.cbg.ExpressionView.events {
	
	import ch.unil.cbg.ExpressionView.model.GeneExpressionData;
	
	import flash.events.Event;
	
	public class UpdateHighlightedModulesEvent extends Event {

		// file data
		public var modulesRectangles:Array;
		public static const UPDATEHIGHLIGHTEDMODULESEVENT:String = "UpdateHighlightedModulesEvent";
		
		public function UpdateHighlightedModulesEvent(_modulesRectangles: Array) {
			super(UPDATEHIGHLIGHTEDMODULESEVENT, true, true);
			modulesRectangles = _modulesRectangles;
		}
		
		override public function clone(): Event {
			return new UpdateHighlightedModulesEvent(modulesRectangles);
		}

	}
}