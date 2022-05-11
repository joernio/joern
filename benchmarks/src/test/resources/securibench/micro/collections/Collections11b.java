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
    
    $Id: Collections11b.java,v 1.1 2006/04/21 17:14:26 livshits Exp $
 */
package securibench.micro.collections;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Collection;

import javax.servlet.ServletResponse;

/** 
 *  @servlet description = "simple collection deposit/retrieve" 
 *  @servlet vuln_count = "1" 
 *  */
class Collections11b {
    protected void foo(Object o, ServletResponse resp) throws IOException {
    	Collection c = (Collection) o;
        String str = c.toString();
        PrintWriter writer = resp.getWriter();  
        writer.println(str);                    /* BAD */
    }
}