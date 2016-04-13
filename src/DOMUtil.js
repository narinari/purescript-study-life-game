"use strict";

// module DOMUtil

exports.removeAttribute = function (name) {
  return function (element) {
    return function () {
      element.removeAttribute(name);
      return {};
    };
  };
};