pragma solidity ^0.8.0;

contract Foo {

    address payable winner = payable(msg.sender);
    bool prizePaidOut;
    bool gameHasEnded;

    function send_payment() public {
        if (gameHasEnded && !(prizePaidOut)) {
            winner.send(1000); // send a prize to the winner
            prizePaidOut = true;
        }
    }

    function safe_send_payment() public {
        if (gameHasEnded && !(prizePaidOut)) {
            if (winner.send(1000))
                prizePaidOut = true;
            else revert();
        }
    }

}