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
    
    $Id: Collections10.java,v 1.1 2006/04/21 17:14:26 livshits Exp $
 */
package securibench.micro.collections;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import securibench.micro.BasicTestCase;
import securibench.micro.MicroTestCase;

/** 
 *  @servlet description = "more complex collection copying" 
 *  @servlet vuln_count = "0" 
 *  */
public class Collections10 extends BasicTestCase implements MicroTestCase {
    private static final String FIELD_NAME = "name";

    protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws IOException {
        String s1 = req.getParameter(FIELD_NAME);
        LinkedList c1 = new LinkedList();
        c1.addLast(s1);
        ArrayList c2 = new ArrayList();
        c2.add("abc");
        
        Iterator iter = c1.iterator();
        PrintWriter writer = resp.getWriter();  
        while(iter.hasNext()){
        	String str = (String) iter.next();
        	
        	writer.println(str);                    /* BAD */
        }
        
        iter = c2.iterator();
        while(iter.hasNext()){
        	String str = (String) iter.next();
        	
        	writer.println(str);                    /* OK */
        }
    }
    
    public String getDescription() {
        return "more complex collection copying";
    }
    
    public int getVulnerabilityCount() {
        return 1;
    }
}