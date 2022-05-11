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
    
    $Id: Basic8.java,v 1.4 2006/04/04 20:00:40 livshits Exp $
 */
package securibench.micro.basic;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.BitSet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import securibench.micro.BasicTestCase;
import securibench.micro.MicroTestCase;

/** 
 *  @servlet description="test of complex conditionals" 
 *  @servlet vuln_count = "1" 
 *  */
public class Basic8 extends BasicTestCase implements MicroTestCase {
    protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws IOException {
        String str = req.getParameter("name");
        BitSet bs = new BitSet(10);
        
        if(bs.get(0)) {
            if(bs.get(1)) {
                if(bs.get(2)) {
                    if(bs.get(3)) {
                        if(bs.get(4)) {
                            
                        }
                    } else {
                        PrintWriter writer = resp.getWriter();
                        writer.println(str);    /* BAD */
                    }
                }
            }
        }
    }
    
    public String getDescription() {
        return "test of complex conditionals";
    }
    
    public int getVulnerabilityCount() {
        return 1;
    }
}