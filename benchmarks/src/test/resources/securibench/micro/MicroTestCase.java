/*
   Copyright 2005 Benjamin Livshits

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
 */
/*
    $Id: MicroTestCase.java,v 1.4 2005/11/26 22:18:19 livshits Exp $
 */
package securibench.micro;

/**
 * An interface all test cases are supposed to implement this interface.
 * 
 * At the top of you case, place the following two keywords:
 * 
 * \@servlet description="..." 
 * \@servlet vuln_count = "1" 
 * 
 * These values will be used by the test harness.
 * */
public interface MicroTestCase {
    public String CONNECTION_STRING = "jdbc:dtF:E. coli;USR=dtfadm;PWD=dtfadm;Create=always;APPL=GIVE;DType=FILE";

    /**
     * A brief textual description of the test case.
     * */
    public String getDescription();
    
    /**
     * Expected number of vulnerabilities in the test case.
     * */
    public int getVulnerabilityCount();
}
