// SPDX-License-Identifier: MIT

pragma solidity ^0.8.0;

import "./StorageOrg.sol";
import "@openzeppelin/contracts/proxy/utils/Initializable.sol";


contract  Organisation  is OrgStorage , Initializable{
    
    modifier onlyRole(bytes32 roleTicker, address account) {

        role.checkRole(roleTicker, account);
        _;
    }

    modifier hasBeenCreated (string memory name ) {
        require(departmentMap[name].departmentContract == address(0), "department has been created already");
        _;
    }

    event CEOGranted(bytes32 CEO, bytes32 CTO, bytes32 CFO, bytes32 HOD, address CEOaddress);
   

/*
    consturctor: takes in the CEO address when minted and grant the address CEO permissions.
    @param  _addressCEO: CEO's address. 

*/
    function __orgs__init (address _addressCEO, address _roleAddress, address _scout) public initializer {
        OrgStorage._addressOrgStorage["CEO"] = _addressCEO;
        OrgStorage._addressOrgStorage["Role"] = _roleAddress;
        OrgStorage.role = Roles (OrgStorage._addressOrgStorage["Role"]);
        grantCEO();
        grantOwner();
        scout = SCOUTCOIN(_scout);
    }
    

/*
    grantCEO: grants the roles for the CEO.
*/    
    function grantCEO() private  {
    /* Roles.ro*/
        OrgStorage.role.grantAllCEO(OrgStorage._addressOrgStorage["CEO"] );
        emit CEOGranted(CEO,CTO,CFO,HOD,OrgStorage._addressOrgStorage["CEO"] );

    }

    function grantChangeCEO() public {
        role.changeCEO(_addressOrgStorage["CEO"], _addressOrgStorage["oldCEO"]);
    }

    function grantOwner() private {
        OrgStorage.role.grantAllOwner(owner());
    }

/*
    changeCEO: takes newCEO address as a parameter and changes the current CEO to the new CEO 
               this function is only able to be called from the owner of all contracts.
    @param  _newCEO: new CEO's address. 

*/
    function changeCEO(address newCEO) public onlyOwner {
        _addressOrgStorage["oldCEO"] = _addressOrgStorage["CEO"];
        _addressOrgStorage["CEO"]  = newCEO;

        grantChangeCEO();

        assert(role.checkCEO(newCEO) == true);
        
    }

/*
    grantCTO: takes an array of addresses and grants the CTO permissions.
    @param  _newCTOs: new CTOS. 

*/
    function grantCTOs(address[] memory _newCTOs) public onlyRole(CEO,_msgSender()) {

        for (uint i = 0; i < _newCTOs.length; i++) {
            if(role.checkCTO(_newCTOs[i]) == false ) {
                role.grantRole(CTO,_newCTOs[i]);
                role.grantRole(HOD,_newCTOs[i]);
                assert(role.checkCTO(_newCTOs[i]) == true);
                assert(role.checkHOD(_newCTOs[i]) == true);
                ctoArray.push(_newCTOs[i]);
                ctoToken[_newCTOs[i]] = ctoCounter;
                ctoCounter ++;
            } else {
                require(role.checkCTO(_newCTOs[i]) == true, "address already CTO");
            }
        }
    }

/*
    removeCTO: takes an array of addresses and removes the CTO permissions.
    @param  _removeCTOs: CTOs to be removed. 

*/
    function removeCTOs(address[] memory _removeCTOs) public onlyRole(CEO,_msgSender()) {
        for (uint i = 0; i < _removeCTOs.length; i++) {
            role.revokeRole(CTO,_removeCTOs[i]);
            role.revokeRole(HOD,_removeCTOs[i]);
            assert(role.checkCTO(_removeCTOs[i]) == false);
            assert(role.checkHOD(_removeCTOs[i]) == false);
            if (ctoToken[_removeCTOs[i]] == ctoCounter - 1) {
                ctoArray.pop();
                ctoCounter--;
            } else {
                address temp = ctoArray[ctoCounter-1];
                ctoToken[temp] = ctoToken[_removeCTOs[i]];
                ctoArray[ctoToken[_removeCTOs[i]]] = temp;
                ctoArray.pop();
                ctoCounter--;
            }
        }
    }
/*
    grantCFO: takes an array of addresses and grants the CFO permissions.
    @param  _newCFOs: new CFOS. 

*/
    function grantCFOs(address[] memory _newCFOs) public onlyRole(CEO,_msgSender()) {
        for (uint i = 0; i<_newCFOs.length; i++) {
            role.grantRole(CFO,_newCFOs[i]);
            assert(role.checkCFO(_newCFOs[i]) == true);
            cfoArray.push(_newCFOs[i]);
            cfoToken[_newCFOs[i]] = cfoCounter;
            cfoCounter++;
        }
    }

/*
    removeCFO: takes an array of addresses and removes the CFO permissions.
    @param  _removeCTOs: CFOs to be removed. 

*/
    function removeCFOs(address[] memory _removeCFOs) public onlyRole(CEO,_msgSender()) {
        for (uint i = 0; i < _removeCFOs.length; i++) {
            role.revokeRole(CFO,_removeCFOs[i]);
            assert(role.checkCFO(_removeCFOs[i]) == false);
            if (cfoToken[_removeCFOs[i]] == cfoCounter - 1) {
                cfoArray.pop();
                cfoCounter--;
            } else {
                address temp = cfoArray[cfoCounter-1];
                cfoToken[temp] = cfoToken[_removeCFOs[i]];
                cfoArray[cfoToken[_removeCFOs[i]]] = temp;
                cfoArray.pop();
                cfoCounter--;
            }
        }
    }


    //TODO: map departments to HODS
    function changeHOD(address _newHOD, string memory deptName) public onlyRole(CTO, _msgSender()) {
        role.revokeRole(HOD, _departmentHOD[deptName] );
        assert(role.checkHOD(_departmentHOD[deptName]) == false);

        _HODtoDept[_departmentHOD[deptName]] = "";
        AddressToDepartment[departmentMap[deptName].HOD] = "";
        departmentMap[deptName].HOD = _newHOD;
        AddressToDepartment[_newHOD] = deptName;
        _departmentHOD[deptName] = _newHOD;
        _HODtoDept[_departmentHOD[deptName]] = keccak256(abi.encodePacked(deptName));
        
        role.grantRole(HOD, _departmentHOD[deptName]);

        assert(role.checkHOD(_departmentHOD[deptName]) == true);
        
    }


/*
    createDepartment: takes an address of HOD and the department name and stores it.
    @param  _removeCTOs: CFOs to be removed. 

*/   
    function createDepartment(address HODaddress, string memory deptName) public onlyRole(CTO,_msgSender()) hasBeenCreated(deptName) {
        require(checkDepartment(deptName) == true, "department name already used");
        departmentMap[deptName] = departDetails(deptName, 0, HODaddress, address(0), departmentCounter);
        _departmentHOD[deptName] = HODaddress;
        AddressToDepartment[HODaddress] = deptName;
        _HODtoDept[HODaddress] =  keccak256(abi.encodePacked(deptName));
        role.grantRole(HOD,HODaddress);
        assert(role.checkHOD(HODaddress) == true);
        departArray.push(departmentMap[deptName].departmentName);
        departmentCounter ++;
        
    }

    function linkDepartment(address _departmentContract, string memory deptName) public onlyRole(CTO, _msgSender()) {
        require(Address.isContract(_departmentContract) , "Address is not a contract");
        departmentMap[deptName].departmentContract = _departmentContract;

    }


/*
    checkDepartment: takes name of department and checks if it used.
    @param  deptName: name of new department. 

*/ 
    function checkDepartment(string memory deptName) private view returns (bool){
        if (departmentMap[deptName].HOD != address(0)) {
            return false;
        } else {
            return true;
        }
    }

    function checkHOD(address _HODAddress, bytes32 deptName) external view returns(bool) {
        if (role.checkCTO(_HODAddress) == true) {
            return true;
        }
        
        if(_HODtoDept[_HODAddress] == deptName) {
            return true;
        } else {
            return false;
        }
    }

    function addDepartmentToStorage(address _department, string memory deptName) public onlyRole(CTO, _msgSender()) {
        _departmentStorage[deptName] = department(_department);
    }
    
    function increaseFunds(address dept, uint amount, string memory deptName) public onlyRole(CFO, _msgSender()) {
        require( checkDepartment(deptName) == false, "Department has not been made yet");
        require( departmentMap[deptName].departmentContract == dept, "incorrect department address given");
        scout.mint(dept, amount);
    }

    function getDepartmentHOD(string memory _deptName) public view returns(address) {
        return (_departmentHOD[_deptName]);
    }

    function getAdmin() public view returns(address) {
        return owner();
    }


    function addToHODMap (address HOD, string memory Name) public  onlyOwner{
     
     AddressToDepartment[HOD] = Name;
    }
    
    

}