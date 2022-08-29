package io.joern.jssrc2cpg.preprocessing

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class EjsPreprocessorTest extends AnyWordSpec with Matchers {

  "EjsPreprocessor" should {
    "replace correctly with script block" in {
      val code =
        """
          |<!DOCTYPE html>
          |<html lang="en">
          |<head>
          |    <% include ../common/head %>
          |</head>
          |<body>
          |    <% include ../common/navigation %>
          |    <div class='container' style='min-height: 450px'><div class='row'><div class='col-md-12'>
          |
          |        <div class='row'>
          |            <div class='col-md-12'>
          |                <div class='page-header'>
          |                    <h2>Admin Dashboard</h2>
          |	                </div>
          |                <div id='admin-body' class='page-body'>
          |                    <a href='/app/admin/users'>List Users</a><br>
          |                </div>
          |                <div id='user-body' class='page-body'>
          |                    You are not an Admin<br>
          |                </div>
          |            </div>
          |         </div>
          |    </div></div></div>
          |    <% include ../common/footer %>
          |</body>
          |    <script>
          |        var isAdmin = <%=admin%>;
          |        if(!isAdmin){
          |            var div = document.getElementById('admin-body');
          |            div.style.display = "none";
          |        }else{
          |            var div = document.getElementById('user-body');
          |            div.style.display = "none";            
          |        }
          |    </script>
          |</html>
          |""".stripMargin

      val expectedCode =
        """
          |               
          |                
          |      
          |                                
          |       
          |      
          |                                      
          |                                                                                             
          |
          |                         
          |                                   
          |                                         
          |                                            
          |                       
          |                                                       
          |                                                                 
          |                      
          |                                                      
          |                                            
          |                      
          |                  
          |               
          |                      
          |                                  
          |       
          |            
          |        var isAdmin =    admin  ;
          |        if(!isAdmin){
          |            var div = document.getElementById('admin-body');
          |            div.style.display = "none";
          |        }else{
          |            var div = document.getElementById('user-body');
          |            div.style.display = "none";            
          |        }
          |             
          |       
          |""".stripMargin
      new EjsPreprocessor().preprocess(code) shouldBe expectedCode
    }

    "replace correctly" in {
      val code =
        """
          |<body>
          |
          |<h1>Welcome <%= user.name %></h1>
          |
          |<b><%_ foo.callWithWhitespaces() _%></b>
          |
          |<b><%- foo.callUnescaped() -%></b>
          |
          |<%# Just a comment here #%>
          |<% if (admin) { %>
          |    <a href="/admin">Admin</a>
          |<% } %>
          |
          |<ul>
          |    <% friends.forEach(function(friend, index) { %>
          |        <li class="<%= index === 0 ? "first" : "" %> <%= friend.name === selected ? "selected" : "" %>"><%= friend.name %></li>
          |    <% }); %>
          |</ul>
          |
          |<%
          |console.log(user);
          |exampleWrite = 'some value';
          |%>
          |
          |</body>
          |""".stripMargin

      val expectedCode =
        """
          |      
          |
          |                user.name ;      
          |
          |       foo.callWithWhitespaces() ;      
          |
          |       foo.callUnescaped() ;      
          |
          |                           
          |   if (admin) {   
          |                              
          |   }   
          |
          |    
          |       friends.forEach(function(friend, index) {   
          |                       index === 0 ? "first" : "" ;      friend.name === selected ? "selected" : "" ;       friend.name ;      
          |       });   
          |     
          |
          |  
          |console.log(user);
          |exampleWrite = 'some value';
          |  
          |
          |       
          |""".stripMargin
      new EjsPreprocessor().preprocess(code) shouldBe expectedCode
    }
  }

}
