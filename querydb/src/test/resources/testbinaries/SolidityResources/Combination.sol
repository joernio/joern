pragma solidity ^0.8.0;

contract Combination {
    address payable owner;
    uint256 public sum;
    function selfDestructSafe() public  {
        require (msg.sender == owner , "not owner");
        selfdestruct(owner);
    }
    function selfDestructUnsafe() public  {
        selfdestruct(owner);
    }

    function change(uint256 a) public {
        sum = a;
    }
}
