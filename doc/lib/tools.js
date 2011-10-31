// -*- mode: javascript; coding: utf-8 -*-

function toggle(elem, text) {
    return function () {
	var ul = getFirstChildByTagName(elem, "ul");
	if (ul == null) return;
	var lis = ul.childNodes;
	for (var j = 0; j < lis.length; j++) {
	    var li = lis.item(j);
	    if (li.nodeType == 1) {
		if (li.style.display == "none") {
		    li.style.display = "block";
		    text.nodeValue = " - ";
		} else {
		    li.style.display = "none";
		    text.nodeValue = " + ";
		}
	    }
	}
    };
}

function getFirstChildByTagName(elem, tag) {
    var nodes = elem.childNodes;
    for (var i = 0; i < nodes.length; i++) {
	var e = nodes.item(i);
	if (e.nodeType == 1 && e.tagName.toLowerCase() == tag.toLowerCase()) {
	    return e;
	}
    }
    return null;
}

function setInitialVisibility(contents, style) {
    var ul = getFirstChildByTagName(contents, "ul");
    if (ul == null) return;
    var lis = ul.childNodes;
    for (var j = 0; j < lis.length; j++) {
	var li = lis.item(j);
	if (li.nodeType == 1) {
	    var child = getFirstChildByTagName(li, "ul");
	    if (child != null) {
		var a = getFirstChildByTagName(li, "a");
		var text = document.createTextNode(" + ");
		var click = document.createElement("a");
		click.href = "javascript:void(0)";
		click.appendChild(text);
		click.onclick = toggle(li, text);
		li.insertBefore(click, a);
	    }
	    li.style.display = style;
	    setInitialVisibility(li, "none");
	}
    }
}

window.onload = function () {
    var contents = document.getElementById("table-of-contents");
    setInitialVisibility(contents, "block");
}
