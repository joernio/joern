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
    
    $Id: Basic5.java,v 1.5 2006/04/04 20:00:40 livshits Exp $
 */
package securibench.micro.basic;

import java.io.IOException;
import java.io.PrintWriter;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import securibench.micro.BasicTestCase;
import securibench.micro.MicroTestCase;

/** 
 *  @servlet description="moderately complex test of derived strings" 
 *  @servlet vuln_count = "3" 
 *  */
public class Basic5 extends BasicTestCase implements MicroTestCase {
    protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws IOException {
        String s1 = req.getParameter("name");
        String s2 = s1.toUpperCase();
        String s3 = s2.concat(";");
        String s4 = s3.replace(';', '.');
        String s5 = s4.trim();
        PrintWriter writer = resp.getWriter();
        
        writer.println(s3);    /* BAD */
        writer.println(s4);    /* BAD */
        writer.println(s5);    /* BAD */
    }
    
    public String getDescription() {
        return "moderately complex test of derived strings";
    }
    
    public int getVulnerabilityCount() {
        return 3;
    }
}