pragma solidity ^0.8.0;

contract Foo {
    address payable owner;
    uint256 public sum;
    function selfDestructSafe() public  {
        require (msg.sender == owner , "not owner");
        selfdestruct(owner);
    }
    function selfDestructUnsafe() public  {
        selfdestruct(owner);
    }

}
