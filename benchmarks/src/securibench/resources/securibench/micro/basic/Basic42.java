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
    
    $Id$
 */
package securibench.micro.basic;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Enumeration;
import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import securibench.micro.BasicTestCase;
import securibench.micro.MicroTestCase;

/** 
 *  @servlet description="use getInitParameterNames" 
 *  @servlet vuln_count = "1" 
 *  */
public class Basic42 extends BasicTestCase implements MicroTestCase {
    protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws IOException {        
        ServletContext context = getServletConfig().getServletContext();
        Enumeration e = context.getInitParameterNames();
        while(e.hasMoreElements()) {
            String name = (String) e.nextElement();
            Object value = context.getInitParameter(name); 
            PrintWriter writer = resp.getWriter();
            writer.println(value.toString());          					 /* BAD */
        }
    }
    
    public String getDescription() {
        return "use getInitParameterNames";
    }
    
    public int getVulnerabilityCount() {
        return 1;
    }
}