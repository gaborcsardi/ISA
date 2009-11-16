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

package ch.unil.cbg.ExpressionView.view.components {
	
	import flash.events.MouseEvent;
	import flash.net.URLRequest;
	import flash.net.navigateToURL;
	
	import mx.controls.Label;
	import mx.controls.listClasses.IDropInListItemRenderer;
	import mx.core.IDataRenderer;

	public class LinkRenderer extends Label implements IDataRenderer, IDropInListItemRenderer {

		public var dataProvider:String;
		private var shortOrganism:String;
				
		public function LinkRenderer() {
			super();
			shortOrganism = new String();
			dataProvider = new String();
		}

		public function set organism(longname:String):void {
			var temp:String = longname.toLowerCase()
			switch (temp) {
				case "homo sapiens":
					shortOrganism = "hsa";
					break;
				case "mus musculus":
					shortOrganism = "mmu";
					break;	
			}
		}
				
		override protected function commitProperties():void{
			this.styleName = "linkrenderer";
		}

		override protected function createChildren():void {
			super.createChildren();
			addEventListener(MouseEvent.MOUSE_OVER, mouseOverHandler);
			addEventListener(MouseEvent.MOUSE_OUT, mouseOutHandler);
			addEventListener(MouseEvent.CLICK, mouseClickHandler);
		}
		
		private function mouseOverHandler(event:MouseEvent):void {
		   event.currentTarget.setStyle("color","0x0000FF");
		   event.currentTarget.setStyle("textDecoration","underline");
		}
 
		private function mouseOutHandler(event:MouseEvent):void {
		   event.currentTarget.setStyle("color","0x000000");
		   event.currentTarget.setStyle("textDecoration","none");
		}
 
		private function mouseClickHandler(event:MouseEvent):void {
			var url:String = new String();
			if ( dataProvider == "genecard" ) {
				url = "http://www.genecards.org/cgi-bin/carddisp.pl?gene=" + event.currentTarget.data.symbol;
			} else if ( dataProvider == "entrez" ) {
				url = "http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene&cmd=Retrieve&dopt=full_report&list_uids=" + event.currentTarget.data.entrezid; 
			} else if ( dataProvider == "go" ) {
				url = "http://amigo.geneontology.org/cgi-bin/amigo/term-details.cgi?term=" + event.currentTarget.data.go
			} else if ( dataProvider == "kegg" ) {
				var tag:String = shortOrganism;
				if ( shortOrganism == "" ) { 
					tag = "map";
				} 
				url = "http://www.genome.jp/dbget-bin/www_bget?" + shortOrganism + event.currentTarget.data.kegg;
			}
			var request:URLRequest = new URLRequest(url);
			navigateToURL(request, "_blank");
		}
				
	}
}