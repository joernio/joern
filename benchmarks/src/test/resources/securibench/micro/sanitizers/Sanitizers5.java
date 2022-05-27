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
    
    $Id: Sanitizers5.java,v 1.5 2006/04/21 17:14:27 livshits Exp $
 */
package securibench.micro.sanitizers;

import java.io.IOException;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.util.Locale;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import securibench.micro.BasicTestCase;
import securibench.micro.MicroTestCase;

/** 
 *  @servlet description="encode and then decode" 
 *  @servlet vuln_count = "1" 
 *  */
public class Sanitizers5 extends BasicTestCase implements MicroTestCase {
    private static final String FIELD_NAME = "name";

    protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws IOException {
        String s = req.getParameter(FIELD_NAME);
        String name = s.toLowerCase(Locale.UK);
        String enc = URLEncoder.encode("/user/" + name, "UTF-8");
        String dec = URLDecoder.decode(enc, "UTF-8");

        resp.sendRedirect(dec);     								/* BAD */
        resp.sendRedirect(enc);    									/* OK */
    }
    
    public String getDescription() {
        return "encode and then decode";
    }
    
    public int getVulnerabilityCount() {
        return 1;
    }
}