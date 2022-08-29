pragma solidity ^0.4.8;

contract Reentrancy {

    mapping (address => uint) balances;

    function contribute() external payable {
        balances[msg.sender] += msg.value;
    }

    function withdraw () public {
        if(balances[msg.sender]== 0) {
            throw;
        }

        if(msg.sender.call.value(balances[msg.sender]) ()){
            balances[msg.sender] = 0;
        } else {
            throw;
        }
    }
    function getFunds () public returns(uint){
        return address(this).balance;
    }
}