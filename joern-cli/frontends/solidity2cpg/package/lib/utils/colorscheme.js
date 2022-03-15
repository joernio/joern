"use strict";

Object.defineProperty(exports, "__esModule", {
    value: true
});
var defaultColorScheme = exports.defaultColorScheme = {
    digraph: {
        bgcolor: undefined,
        nodeAttribs: {},
        edgeAttribs: {}
    },
    visibility: {
        public: "green",
        external: "blue",
        private: "red",
        internal: "white"
    },
    nodeType: {
        modifier: "yellow"
    },
    call: {
        default: "orange",
        regular: "green",
        this: "green"
    },
    event: {
        style: "dotted"
    },
    contract: {
        defined: {
            bgcolor: "lightgray",
            color: "lightgray"
        },
        undefined: {
            bgcolor: undefined,
            color: "lightgray"
        }
    }
};

var defaultColorSchemeDark = exports.defaultColorSchemeDark = {
    digraph: {
        bgcolor: "#2e3e56",
        nodeAttribs: {
            style: "filled",
            fillcolor: "#edad56",
            color: "#edad56",
            penwidth: "3"
        },
        edgeAttribs: {
            color: "#fcfcfc",
            penwidth: "2",
            fontname: "helvetica Neue Ultra Light"
        }
    },
    visibility: {
        isFilled: true,
        public: "#FF9797",
        external: "#ffbdb9",
        private: "#edad56",
        internal: "#f2c383"
    },
    nodeType: {
        isFilled: false,
        shape: "doubleoctagon",
        modifier: "#1bc6a6",
        payable: "brown"
    },
    call: {
        default: "white",
        regular: "#1bc6a6",
        this: "#80e097"
    },
    event: {
        style: "dotted"
    },
    contract: {
        defined: {
            bgcolor: "#445773",
            color: "#445773",
            fontcolor: "#f0f0f0",
            style: "rounded"
        },
        undefined: {
            bgcolor: "#3b4b63",
            color: "#e8726d",
            fontcolor: "#f0f0f0",
            style: "rounded,dashed"
        }
    }

};