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
    
    $Id: Inter4.java,v 1.4 2006/04/04 20:00:40 livshits Exp $
 */
package securibench.micro.inter;

import java.io.IOException;
import java.io.PrintWriter;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import securibench.micro.BasicTestCase;
import securibench.micro.MicroTestCase;

/** 
 *  @servlet description="store stuff in a field" 
 *  @servlet vuln_count = "1" 
 *  */
public class Inter4 extends BasicTestCase implements MicroTestCase {
    private static final String FIELD_NAME = "name";
    private String name;

    protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws IOException {
        name = req.getParameter(FIELD_NAME);

        f(resp);
    }
    
    private void f(ServletResponse resp) throws IOException {
        PrintWriter writer = resp.getWriter();
        writer.println(this.name);         /* BAD */        
    }

    public String getDescription() {
        return "store stuff in a field";
    }
    
    public int getVulnerabilityCount() {
        return 2;
    }
}