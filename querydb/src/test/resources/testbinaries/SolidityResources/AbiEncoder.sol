pragma solidity 0.4.7;

contract A {
    uint[2][3] bad_arr = [[1, 2], [3, 4], [5, 6]];

    /* Array of arrays passed to abi.encode is vulnerable */
    function bad() public {
        bytes memory b = abi.encode(bad_arr);
    }
}
