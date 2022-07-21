pragma solidity ^0.8.0;
contract Delegatecall{
    function delegate(address to, bytes data){
        to.delegatecall(data);
    }
}