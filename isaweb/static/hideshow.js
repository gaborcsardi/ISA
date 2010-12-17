function toggle(id) {
    var ele = document.getElementById("toggle"+id);
    var text = document.getElementById("display"+id);
    if(ele.style.display == "block") {
    	ele.style.display = "none";
	text.innerHTML = "&#9658;";
    }
    else {
	ele.style.display = "block";
	text.innerHTML = "&#9660;";
    }
} 

function expandAll() {
    var allele = document.getElementsByClassName('hideable');
    var alltext = document.getElementsByClassName('hideabletext'); 
    for (i = 0; i < allele.length; i++) {
	allele[i].style.display = "block";
    }
    for (i = 0; i < alltext.length; i++) {
	alltext[i].innerHTML = "&#9660;";
    }
}

function collapseAll() {
    var allele = document.getElementsByClassName('hideable');
    var alltext = document.getElementsByClassName('hideabletext'); 
    for (i = 0; i < allele.length; i++) {
	allele[i].style.display = "none";
    }
    for (i = 0; i < alltext.length; i++) {
	alltext[i].innerHTML = "&#9658;";
    }
}
