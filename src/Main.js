"use strict";

exports.setInnerText = function(element) {
    return function(newContent) {
        element.innerText = newContent;
        return undefined;
    }
}