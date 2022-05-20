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
    
    $Id: Basic9.java,v 1.4 2006/04/04 20:00:40 livshits Exp $
 */
package securibench.micro.basic;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Random;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import securibench.micro.BasicTestCase;
import securibench.micro.MicroTestCase;

/** 
 *  @servlet description="chains of value assignments" 
 *  @servlet vuln_count = "1" 
 *  */
public class Basic9 extends BasicTestCase implements MicroTestCase {
    protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws IOException {
        String s1 = req.getParameter("name");
        Random r = new Random();
        boolean choice = r.nextBoolean();
        String s2 = s1;
        String s3 = s2;
        //String s4 = s1;
        String s5 = choice ? s3 : s2;
        String s6 = s5;
        
        PrintWriter writer = resp.getWriter();
        writer.println(s6);    /* BAD */
    }
    
    public String getDescription() {
        return "chains of value assignments";
    }
    
    public int getVulnerabilityCount() {
        return 1;
    }
}