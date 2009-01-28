function showstuff(boxid){
   document.getElementById(boxid).style.visibility="visible";
   document.getElementById(boxid).style.display="block";
}
function hidestuff(boxid){
   document.getElementById(boxid).style.visibility="hidden";
   document.getElementById(boxid).style.display="none";
}
function isArray(testObject) {   
    return testObject && !(testObject.propertyIsEnumerable('length')) && typeof testObject === 'object' && typeof testObject.length === 'number';
}
function QuickUpdate() {
   var what=document.getElementById("quick.select").value;
   document.getElementById("tooltip.select").value=what;   
   document.getElementById("main.select").value=what;
   document.getElementById("color.select").value=what;
   document.getElementById("right.select").value=what;
   MyUpdate([ 'tooltip', 'main', 'color', 'right' ]);
}
function MainUpdate() {
   var what=document.getElementById("cth.select").value;
   if (what != "select") {
      window.location="../html-" + what + "/maintree.html";
   }
}
function MyUpdate(args){
   if (!isArray(args)) {
     args=[ args ];
   }
   var ii=0;
   while (args.length > ii) {
     var what=args[ii];
     var elem=document.getElementById(what+'.select');
     var value=elem.value;
     var opts=elem.options;
     var i=0;
     while (opts.length > i) {
       hidestuff("main"+what+opts[i].value);
       i++;
     }
     showstuff("main"+what+value);
     ii++;
   }
}
var foobar= {
   init: function() {
     MyUpdate([ 'main', 'tooltip', 'left', 'right', 'color' ]);
   }
}
if (document.addEventListener) {
  document.addEventListener("DOMContentLoaded", foobar.init, null);
}
