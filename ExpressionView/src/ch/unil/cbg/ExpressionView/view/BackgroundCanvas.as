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

	import ch.unil.cbg.ExpressionView.events.*;
	
	import flash.events.MouseEvent;
	import flash.net.URLRequest;
	import flash.net.navigateToURL;
	
	import mx.containers.Canvas;
	import mx.containers.HBox;
	import mx.containers.VBox;
	import mx.controls.HRule;
	import mx.controls.Image;
	import mx.controls.Text;
			
	public class BackgroundCanvas extends Canvas {

		private var box:VBox;
		private var expressionviewlogo:Text;
		private var about:Text;
		private var rule:HRule;
		private var logos:HBox;
		private var cbg:Image;
		private var unil:Image;
		private var sib:Image;
		
		private var showBackground:Boolean;;
		
		public function BackgroundCanvas() {
			super();
			showBackground = true;
		}
		        
		// layout		
		override protected function createChildren():void {
			super.createChildren();
			
			if ( !box ) { 
				box = new VBox();
				box.setStyle("horizontalAlign","center");
				box.setStyle("verticalAlign","middle");
				box.setStyle("verticalGap","30");
				addChild(box);
				
				if ( !expressionviewlogo ) {
					expressionviewlogo = new Text();
					expressionviewlogo.htmlText = "<p align='center'><img src='ch/unil/cbg/ExpressionView/assets/logos/expressionviewlogo.png' width='60' height='60' align='left'>" +
												 "<font size='0'><br></p><p><font family='Arial' color='#000000' size='40'>       ExpressionView</p>";
					box.addChild(expressionviewlogo);
				}
										
				if ( !about ) {
					about = new Text();
					var format:String = "<font family='Verdana' color='#000000' size='12'/>";
					about.htmlText = "<p align='center'>" + format + "This software has been developed by the<br><a href='http://www2.unil.ch/cbg/' target='_blank'><b>Computational Biology Group</b></a> at the " + 
							"<a href='http://www.unil.ch' target='_blank'><b>University of Lausanne</b></a><br>and the <a href='http://www.isb-sib.ch/' target='_blank'><b>Swiss Institute of Bioinformatics</b></a>.</p>";								
					box.addChild(about);
				}
	
				if ( !rule ) {
					rule = new HRule();
					box.addChild(rule);
				}
	
				if ( !logos ) {
					logos = new HBox()
					logos.setStyle("horizontalAlign", "center");
					logos.setStyle("horizontalGap", "30");
					box.addChild(logos);
					
					if ( !cbg ) {
						cbg = new Image();
						cbg.name = "cbg";
						cbg.source = "ch/unil/cbg/ExpressionView/assets/logos/cbglogo.png";
						cbg.toolTip = "Computational Biology Group";
						cbg.useHandCursor = true;
						cbg.buttonMode = true;
						cbg.addEventListener(MouseEvent.CLICK, clickHandler);
						logos.addChild(cbg);
					}
					if ( !unil ) {
						unil = new Image();
						unil.name = "unil";
						unil.source = "ch/unil/cbg/ExpressionView/assets/logos/unillogo.png";
						unil.toolTip = "University of Lausanne";
						unil.useHandCursor = true;
						unil.buttonMode = true;
						unil.addEventListener(MouseEvent.CLICK, clickHandler);
						logos.addChild(unil);
					}
					if ( !sib ) {
						sib = new Image();
						sib.name = "sib";
						sib.source = "ch/unil/cbg/ExpressionView/assets/logos/siblogo.png";
						sib.toolTip = "Swiss Institute of Bioinformatics";
						sib.useHandCursor = true;
						sib.buttonMode = true;
						sib.addEventListener(MouseEvent.CLICK, clickHandler);
						logos.addChild(sib);
					}
				}

			}

		}
		
		override protected function updateDisplayList(unscaledWidth:Number, unscaledHeight:Number):void {
			super.updateDisplayList(unscaledWidth, unscaledHeight);
		
			if ( showBackground ) {
				
				box.percentWidth = 100;
				box.percentHeight = 100;
				
				var w:Number = Math.max(about.measuredWidth, logos.measuredWidth, expressionviewlogo.measuredWidth)
				about.width = w
				rule.width = w
				logos.width = w
				cbg.height = 150;
				cbg.width = 150;
				unil.height = 160;
				unil.width = 160;
				sib.height = 90;
				sib.width = 90;
			}

			parentApplication.dispatchEvent(new UpdateStatusBarEvent("click Open to select the ExpressionView data you want to explore."));
			
		}
		
		public function set show(_show: Boolean): void {
			showBackground = _show;
			box.visible = showBackground;
		}
		
		private function clickHandler(event:MouseEvent):void {
			var url:String = "http://www2.unil.ch/cbg";
			if ( event.currentTarget.name == "sib" ) {
				url = "http://www.isb-sib.ch/";
			} else if ( event.currentTarget.name == "unil" ) {
				url = "http://www.unil.ch";
			}
			var urlRequest:URLRequest = new URLRequest(url);
			navigateToURL(urlRequest, "_blank");

		}
		
	}
}