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
    
    $Id: Refl3.java,v 1.6 2006/04/04 20:00:40 livshits Exp $
 */
package securibench.micro.reflection;

import java.io.IOException;
import java.io.PrintWriter;
import java.lang.reflect.Field;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import securibench.micro.BasicTestCase;
import securibench.micro.MicroTestCase;

/** 
 *  @servlet description = "reflectively create a class and access its field" 
 *  @servlet vuln_count = "1" 
 *  */
public class Refl3 extends BasicTestCase implements MicroTestCase {
    private static final String FIELD_NAME = "name";
    private String name;
    
    public static class ReflectivelyCreated {
        public String value;        
    }

    protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws IOException {
        name = req.getParameter(FIELD_NAME);
        PrintWriter writer = resp.getWriter();
        
        try {
            Class clazz = Class.forName("securibench.micro.reflection.Refl3$ReflectivelyCreated");
            ReflectivelyCreated rc = (ReflectivelyCreated) clazz.newInstance();
            Field field = clazz.getField("value");
            field.set(rc, name);
            
            writer.println(rc.value);               /* BAD */
        } catch (ClassNotFoundException e) {
            System.err.println("An error occurred (1)");
        } catch (InstantiationException e) {
            System.err.println("An error occurred (2)");
        } catch (IllegalAccessException e) {
            System.err.println("An error occurred (3)");
        } catch (SecurityException e) {
            System.err.println("An error occurred (4)");
        } catch (NoSuchFieldException e) {
            System.err.println("An error occurred (5)");
        }
    }

    public String getDescription() {
        return "reflectively create a class and access its field";
    }
    
    public int getVulnerabilityCount() {
        return 1;
    }
}