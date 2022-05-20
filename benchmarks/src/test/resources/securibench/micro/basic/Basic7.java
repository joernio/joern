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
    
    $Id: Basic7.java,v 1.4 2006/04/04 20:00:40 livshits Exp $
 */
package securibench.micro.basic;

import java.io.IOException;
import java.io.PrintWriter;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import securibench.micro.BasicTestCase;
import securibench.micro.MicroTestCase;

/** 
 *  @servlet description="complex test of derived strings involving a string buffer" 
 *  @servlet vuln_count = "1" 
 *  */
public class Basic7 extends BasicTestCase implements MicroTestCase {
    protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws IOException {
        String s1 = req.getParameter("name");
        String s2 = s1.toUpperCase();
        StringBuffer buf = new StringBuffer(s2);
        buf.append("abcdefgh");
        buf.insert(3, 's');
        String s3 = buf.toString();
        
        PrintWriter writer = resp.getWriter();
        
        writer.println(s3);    /* BAD */
    }
    
    public String getDescription() {
        return "complex test of derived strings involving a string buffer";
    }
    
    public int getVulnerabilityCount() {
        return 1;
    }
}