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
    
    $Id: Sanitizers6.java,v 1.4 2006/04/04 20:00:41 livshits Exp $
 */
package securibench.micro.sanitizers;

import java.io.IOException;
import java.io.PrintWriter;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import securibench.micro.BasicTestCase;
import securibench.micro.MicroTestCase;

/** 
 *  @servlet description="sanitizers for directory traversal" 
 *  @servlet vuln_count = "0" 
 *  */
public class Sanitizers6 extends BasicTestCase implements MicroTestCase {
    private static final String FIELD_NAME = "name";
    private PrintWriter writer;

    protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws IOException {
        String name = req.getParameter(FIELD_NAME);
        String clean = clean(name);
        
        writer = resp.getWriter();
        resp.setContentType("text/html");
        
        writer.println("<html>" + clean + "</html>");                  /* OK */        
    }
    
    /** 
     * @sanitizer 
     * sanitization routine for removing . and /\ characters from strings.
     * This routine performs white-listing by only allowing letters and digits through.  
     * */
    private static String clean(String name) {
        StringBuffer buf = new StringBuffer();
        for(int i = 0; i < name.length(); i++) {
            char ch = name.charAt(i);
            
            if(Character.isLetter(ch) || Character.isDigit(ch) || ch == '_') {
                buf.append(ch);
            } else {
                buf.append('?');
            }
        }
        
        return buf.toString();
    }

    public String getDescription() {
        return "simple sanitization check";
    }
    
    public int getVulnerabilityCount() {
        return 1;
    }
    
    public static void main(String[] args) {
        System.out.println(clean("xx/../yy"));  // xx????yy
        System.out.println(clean("~xx"));       // ?xx
        System.out.println(clean("xx_yy"));     // xx_yy
    }
}