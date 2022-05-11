/*
   Copyright 2006 Benjamin Livshits

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

/**
    @author Benjamin Livshits <livshits@cs.stanford.edu>
    
    $Id: Factories3.java,v 1.3 2006/04/04 20:00:41 livshits Exp $
 */
package securibench.micro.factories;

import java.io.IOException;
import java.io.PrintWriter;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import securibench.micro.BasicTestCase;
import securibench.micro.MicroTestCase;

/** 
 *  @servlet description = "factory problem with a string wrapper" 
 *  @servlet vuln_count = "1" 
 *  */
public class Factories3 extends BasicTestCase implements MicroTestCase {
    class StringWrapper {
        StringWrapper(String value){
            this.value = value;
        }
        public String toString() {
            return value;
        }
        
        protected String value;
    }
    
    protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws IOException {
        String s1 = req.getParameter("name");
        
        StringWrapper w1 = new StringWrapper(s1);
        StringWrapper w2 = new StringWrapper("abc");
            
        PrintWriter writer = resp.getWriter();
        
        writer.println(w1.toString());    /* BAD */
        writer.println(w2.toString());    /* OK */
    }
    
    public String getDescription() {
        return "factory problem with a string wrapper";
    }

    public int getVulnerabilityCount() {
        return 1;
    }
}