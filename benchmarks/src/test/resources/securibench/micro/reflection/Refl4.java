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
    
    $Id: Refl4.java,v 1.5 2006/04/04 20:00:41 livshits Exp $
 */
package securibench.micro.reflection;

import java.io.IOException;
import java.io.PrintWriter;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import securibench.micro.BasicTestCase;
import securibench.micro.MicroTestCase;

/** 
 *  @servlet description = "bug in class initializer" 
 *  @servlet vuln_count = "1" 
 *  */
public class Refl4 extends BasicTestCase implements MicroTestCase {
    private static final String FIELD_NAME = "name";
    private static String name;
    private static PrintWriter writer;
    
    static class ReflectivelyCreated {
        static {
            writer.println(name);               /* BAD */
            
        }
    }
    
    protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws IOException {
        name = req.getParameter(FIELD_NAME);
        writer = resp.getWriter();
        
        try {
            // this invokes the class initializer
            Class.forName("securibench.micro.reflection.Refl4$ReflectivelyCreated");
        } catch (ClassNotFoundException e) {
            System.err.println("An error occurred (1)");
        } catch (SecurityException e) {
            System.err.println("An error occurred (2)");
        }
    }

    public String getDescription() {
        return "bug in class initializer";
    }
    
    public int getVulnerabilityCount() {
        return 1;
    }
}