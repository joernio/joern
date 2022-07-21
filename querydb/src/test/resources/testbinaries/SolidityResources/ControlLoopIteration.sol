contract C{
    uint n;

    function set(uint _n) public{
        n = _n;
    }

    function bad() public{
        uint i;
        uint counter;
        for(i=0; i<n; i++){
            counter = i;
        }
    }
}