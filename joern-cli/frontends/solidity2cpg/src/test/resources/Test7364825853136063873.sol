
 // SPDX-License-Identifier: GPL-3.0
 pragma solidity ^0.8.0;

 contract Additive{

    function Foo(uint256 argc, string memory argv ) public pure {
        int256 a = 3;
        int256 b = 2.0;
        int256 c = a + b;
        int256 e = a * b;
        int256 f = b/a;
    }

 }
    