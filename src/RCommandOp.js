"use strict";

//module RCommandOp

exports.rcOpf =
    function (rootfile , funccode) {return process.compile(funccode,'./plugin/' + rootfile);}
