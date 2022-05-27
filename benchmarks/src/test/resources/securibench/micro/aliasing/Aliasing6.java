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
 *//**
    @author Benjamin Livshits <livshits@cs.stanford.edu>
    
    $Id: Aliasing6.java,v 1.1 2006/04/21 17:14:27 livshits Exp $
 */
package securibench.micro.aliasing;

import java.io.IOException;
import java.io.PrintWriter;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import securibench.micro.BasicTestCase;
import securibench.micro.MicroTestCase;

/** 
 *  @servlet description="aliasing with copy propagation" 
 *  @servlet vuln_count = "7" 
 *  */
public class Aliasing6 extends BasicTestCase implements MicroTestCase {
	private static final String FIELD_NAME = "name";

    protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws IOException {
       String[] names = req.getParameterValues(FIELD_NAME);
       Object 
       	o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14, o15, o16, o17, o18, o19, o20,
       	o21, o22, o23, o24, o25, o26, o27, o28, o29, o30, o31, o32, o33, o34, o35, o36, o37, o38, o39, o40;
       o1 = o2 = o3 = o4 = o5 = o6 = o7 = o8 = o9 = o10 = o11 = o12 = o13 = o14 = o15 = o16 = o17 = o18 = o19 = o20 =
  	   o21 = o22 = o23 = o24 = o25 = o26 = o27 = o28 = o29 = o30 = o31 = o32 = o33 = o34 = o35 = o36 = o37 = o38 = o39 = o40 =
  		   names[0];
              
       PrintWriter writer = resp.getWriter();
       writer.println(o1);                              /* BAD */
       writer.println(o2);                              /* BAD */
       writer.println(o3);                              /* BAD */
       writer.println(o4);                              /* BAD */
       writer.println(o32);                             /* BAD */
       writer.println(o37);                             /* BAD */
       writer.println(o40);                             /* BAD */
    }
    
    public String getDescription() {
        return "aliasing with copy propagation";
    }
    
    public int getVulnerabilityCount() {
        return 7;
    }
}