// -*- mode: javascript; coding: utf-8 -*-
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
    var wrapper = document.getElementsByTagName("body").item(0);
    wrapper.insertBefore(topAnchor, wrapper.firstChild);
}

window.onload = function () {
    addNavigator();
}
