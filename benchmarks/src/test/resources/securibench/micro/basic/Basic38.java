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
   
   $Id: Basic38.java,v 1.2 2006/04/04 20:00:40 livshits Exp $
 */
package securibench.micro.basic;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.StringTokenizer;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import securibench.micro.BasicTestCase;
import securibench.micro.MicroTestCase;

/**
 * @servlet description="StringTokenizer test with a false positive"
 * @servlet vuln_count = "1"
 */
public class Basic38 extends BasicTestCase implements MicroTestCase {
    private static final String FIELD_NAME = "name";
      
    protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws IOException {
        String name = req.getParameter(FIELD_NAME);
        StringTokenizer tok1 = new StringTokenizer(name, "\t");
        StringTokenizer tok2 = new StringTokenizer("abc\tdef", "\t");
        
        while(tok1.hasMoreTokens() && tok2.hasMoreElements()) {
            PrintWriter writer = resp.getWriter();        
            writer.println(tok1.nextToken());              /* BAD */
            writer.println(tok2.nextToken());              /* OK */
        }
    }

    public String getDescription() {
        return "StringTokenizer test with a false positive";
    }

    public int getVulnerabilityCount() {
        return 1;
    }
}