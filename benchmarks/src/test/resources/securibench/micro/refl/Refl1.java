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
     
     $Id: Refl1.java,v 1.4 2006/04/04 20:00:41 livshits Exp $
 */
package securibench.micro.reflection;

import java.io.IOException;
import java.io.PrintWriter;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import securibench.micro.BasicTestCase;
import securibench.micro.MicroTestCase;

/** 
 *  @servlet description="reflective call of a method" 
 *  @servlet vuln_count = "1" 
 *  */
public class Refl1 extends BasicTestCase implements MicroTestCase {
    private static final String FIELD_NAME = "name";

    protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws IOException {
        String s1 = req.getParameter(FIELD_NAME);        
        PrintWriter writer = resp.getWriter();
        
        Method idMethod = null;
        try {
            Class clazz = Class.forName("securibench.micro.reflection.Refl1");
            Method methods[] = clazz.getMethods(); 
            for(int i = 0; i < methods.length; i++) {
                Method method = methods[i];
                if(method.getName().equals("id")) {
                    idMethod = method;
                    break;
                }
            }
            // a fancy way to call id(s1, writer)
            Object o = idMethod.invoke(this, new Object[] {s1, writer});
            String s2 = (String) o;
            writer.println(s2);         /* BAD */ 
        } catch( ClassNotFoundException e ) {
            e.printStackTrace();
        } catch (IllegalArgumentException e) {
            e.printStackTrace();
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        } catch (InvocationTargetException e) {
            e.printStackTrace();
        }         
    }
    
    public String id(String string, PrintWriter writer) {
        return string;
    }

    public String getDescription() {
        return "reflective call of a method";
    }
    
    public int getVulnerabilityCount() {
        return 1;
    }
}