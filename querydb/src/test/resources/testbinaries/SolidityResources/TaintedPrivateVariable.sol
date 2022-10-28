
pragma solidity ^0.8.0;

contract Foo {

    uint private a ;

    function editVariable(uint _a) public {
        a = _a;
    }

    function confirmOwner()
    public
    {
        if(msg.sender==newOwner)
        {
            owner=newOwner;
        }
    }

}