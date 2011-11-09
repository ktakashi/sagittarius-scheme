// -*- mode: javascript; coding: utf-8 -*-

function toggle(elem, link) {
    return function () {
	var ul = getFirstChildByTagName(elem, "ul");
	if (ul == null) return;
	var lis = ul.childNodes;
	for (var j = 0; j < lis.length; j++) {
	    var li = lis.item(j);
	    if (li.nodeType == 1) {
		if (li.style.display == "none") {
		    li.style.display = "block";
		    link.className = "opened"
		} else {
		    li.style.display = "none";
		    link.className = "closed"
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
	    var a = getFirstChildByTagName(li, "a");
	    var click = document.createElement("a");
	    click.href = "javascript:void(0)";
	    if (child != null) {
		click.className = "closed";
		click.onclick = toggle(li, click);
	    } else {
		click.className = "no-child";
	    }
	    li.insertBefore(click, a);
	    li.style.display = style;
	    setInitialVisibility(li, "none");
	}
    }
}

function addNavigator() {
    var top = document.createElement("a");
    var index = document.createElement("a");
    var back = document.createElement("a");
    var forward = document.createElement("a");
    var topAnchor = document.createElement("a");
    var div = document.createElement("div");
    var first = document.body.firstChild;
    top.className = "navigator";
    top.appendChild(document.createTextNode("Top"));
    index.className = "navigator";
    index.appendChild(document.createTextNode("Index"));
    back.className = "navigator";
    back.appendChild(document.createTextNode("<<"));
    forward.className = "navigator";
    forward.appendChild(document.createTextNode(">>"));
    top.href = "#top-anchor";
    index.href = "#index";
    back.href = "javascript:history.back()";
    forward.href = "javascript:history.forward()";
    topAnchor.name = "top-anchor"
    div.className = "navigator"

    div.appendChild(back);
    div.appendChild(forward);
    div.appendChild(top);
    div.appendChild(index);
    document.body.appendChild(div);
    var wrapper = document.getElementById("sagittarius-doc-wrapper");
    wrapper.insertBefore(topAnchor, wrapper.firstChild);
}

window.onload = function () {
    var contents = document.getElementById("table-of-contents");
    setInitialVisibility(contents, "block");
    addNavigator();
}
