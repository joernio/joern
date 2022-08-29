// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;
import "@openzeppelin/contracts/access/Ownable.sol";
import "./Roles.sol";
import "./Organisation.sol";
import "./SCOUTCOIN.sol";
contract DepStorage is Ownable{

    bytes32 public constant CTO = keccak256("CTO");
    bytes32 public constant HOD = keccak256("HOD");
    address public currentDepartment;
    string public name;
    mapping(string => address) public _addressDepStorage; 
    address HODaddress;
    Roles public role;
    SCOUTCOIN scout;
    Organisation public org; 
    bytes32 public departmentName;
    uint256 public infCounter ;
    uint256 public expCounter ;
    uint256 public referalCounter;
    address[] public lstInfNFT;
    address[] public lstExpNFT;
    mapping (string => address[]) public _stringToAddressArray;
    struct expNFT {
        address nftAddress;
        uint256 listPointer;
    }
    

    mapping(string => mapping (address => uint256) ) public _StorageAddressToTokenId;
    mapping(address => expNFT) public MapExpNFT;
    mapping(address => uint256) public MapInfNFT;
    
}