// SPDX-License-Identifier: GPL-3.0
pragma solidity ^0.8.0;

contract Additive {

    function Foo(uint256 argc, string memory argv) public pure {
        uint8 a = 3;
        uint256 b = 2; // no floating points in Solidity (yet)
        int c = a + b;
        uint d = c - a;
        uint e = a * b;
        int256 f = b / a;
    }

}