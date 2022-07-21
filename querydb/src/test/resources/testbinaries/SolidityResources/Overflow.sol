pragma solidity ^0.8.0;

contract overflow {
    int sellerBalance;
    constructor(){
        sellerBalance = 0;
    }
    function add(int value) public  {
        sellerBalance += value; // possible overflow
    }

    function safe_add(int value) public {
        require(value + sellerBalance >= sellerBalance, "Overflow");
        sellerBalance += value;
    }
}