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
    
    $Id: StrongUpdates3.java,v 1.4 2006/04/04 20:00:41 livshits Exp $
 */
package securibench.micro.strong_updates;

import java.io.IOException;
import java.io.PrintWriter;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import securibench.micro.BasicTestCase;
import securibench.micro.MicroTestCase;

/** 
 *  @servlet description="strong updates in data structures" 
 *  @servlet vuln_count = "0" 
 *  */
public class StrongUpdates3 extends BasicTestCase implements MicroTestCase {
    private static final String FIELD_NAME = "name";
    
    class Widget {
        String value = null;
    }

    protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws IOException {
        String name = req.getParameter(FIELD_NAME);
        Widget w = new Widget();
        w.value = name;
        w.value = "abc";

        PrintWriter writer = resp.getWriter();
        writer.println(w.value);              /* OK */
    }

    public String getDescription() {
        return "strong updates in data structures";
    }

    public int getVulnerabilityCount() {
        return 0;
    }    
}