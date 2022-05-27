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
    
    $Id: Collections13.java,v 1.1 2006/04/21 17:14:26 livshits Exp $
 */
package securibench.micro.collections;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.LinkedList;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import securibench.micro.BasicTestCase;
import securibench.micro.MicroTestCase;

/** 
 *  @servlet description = "more complex collection copying through an array" 
 *  @servlet vuln_count = "1" 
 *  */
public class Collections13 extends BasicTestCase implements MicroTestCase {
    private static final String FIELD_NAME = "name";

    protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws IOException {
        String s1 = req.getParameter(FIELD_NAME);
        LinkedList c1 = new LinkedList();
        c1.addLast(s1);
        c1.addFirst("x");
        Object[] array = c1.toArray();
        List c2 = java.util.Arrays.asList(array);
        List c3 = java.util.Arrays.asList(new String[]{new String("xyz")});
        List c4 = java.util.Arrays.asList(new String[]{new String(s1)});
        
        PrintWriter writer = resp.getWriter();  
      	writer.println(c2.get(0));                    /* BAD */
      	writer.println(c3.get(0));                    /* OK */
      	writer.println(c4.get(0));                    /* OK */
    }
    
    public String getDescription() {
        return "more complex collection copying through an array";
    }
    
    public int getVulnerabilityCount() {
        return 1;
    }
}