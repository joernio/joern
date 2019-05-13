package io.shiftleft.joern

import org.scalatest.{Matchers, WordSpec}

class DataFlowTests extends WordSpec with Matchers {

  new TestCpg(
    """| #include <stdlib.h>
       | struct node {
       | int value;
       | struct node *next;
       | };
       |
       | void free_list(struct node *head) {
       | struct node *q;
       | for (struct node *p = head; p != NULL; p = q) {
       |    q = p->next;
       |    free(p);
       |    }
       | }
       | int flow(int p0) {
       |    int a = p0;
       |    int b=a;
       |    int c=0x31;
       |    int z = b + c;
       |    z++;
       |    int x = z;
       |    return x;
       |    }
    """.stripMargin
  ) {
    "should identify all calls to `free`" in {
      cpg.call.name("free").code.toSet shouldBe Set("free(p)")
    }

    "should find flows to arguments of `free`" in {
      val source = cpg.identifier
      val sink = cpg.method.name("free").parameter.argument
      sink.reachableByFlows(source).l.size shouldBe 5
    }

    "should find flows to `free`" in {
      val source = cpg.identifier
      val sink = cpg.call.name("free")
      sink.reachableByFlows(source).l.size shouldBe 5

      // Sample output
      """
  _______________________________________________________________________________________________
 | tracked  | lineNumber| method   | file                                                       |
 |==============================================================================================|
 | *p = head| 9         | free_list| /tmp/dflowtest2108218431997346055/Test1030166153760116596.c|
 | free(p)  | 11        | free_list| /tmp/dflowtest2108218431997346055/Test1030166153760116596.c|

 _________________________________________________________________________________________________
 | tracked    | lineNumber| method   | file                                                       |
 |================================================================================================|
 | *p = head  | 9         | free_list| /tmp/dflowtest2108218431997346055/Test1030166153760116596.c|
 | q = p->next| 10        | free_list| /tmp/dflowtest2108218431997346055/Test1030166153760116596.c|
 | p = q      | 9         | free_list| /tmp/dflowtest2108218431997346055/Test1030166153760116596.c|
 | free(p)    | 11        | free_list| /tmp/dflowtest2108218431997346055/Test1030166153760116596.c|

 _________________________________________________________________________________________________
 | tracked    | lineNumber| method   | file                                                       |
 |================================================================================================|
 | q = p->next| 10        | free_list| /tmp/dflowtest2108218431997346055/Test1030166153760116596.c|
 | p = q      | 9         | free_list| /tmp/dflowtest2108218431997346055/Test1030166153760116596.c|
 | free(p)    | 11        | free_list| /tmp/dflowtest2108218431997346055/Test1030166153760116596.c|

  _____________________________________________________________________________________________
 | tracked| lineNumber| method   | file                                                       |
 |============================================================================================|
 | p = q  | 9         | free_list| /tmp/dflowtest2108218431997346055/Test1030166153760116596.c|
 | free(p)| 11        | free_list| /tmp/dflowtest2108218431997346055/Test1030166153760116596.c|

  _____________________________________________________________________________________________
 | tracked| lineNumber| method   | file                                                       |
 |============================================================================================|
 | free(p)| 11        | free_list| /tmp/dflowtest2108218431997346055/Test1030166153760116596.c|
)"""
    }

    "should find flows from identifiers to return values of `flow`" in {
      val source = cpg.identifier
      val sink = cpg.method.name("flow").methodReturn
      sink.reachableByFlows(source).l.size shouldBe 7

      // Sample output
      """
  ____________________________________________________________________________________________
 | tracked  | lineNumber| method| file                                                       |
 |===========================================================================================|
 | c=0x31   | 17        | flow  | /tmp/dflowtest4491306753443006638/Test8956901754875487242.c|
 | b + c    | 18        | flow  | /tmp/dflowtest4491306753443006638/Test8956901754875487242.c|
 | z = b + c| 18        | flow  | /tmp/dflowtest4491306753443006638/Test8956901754875487242.c|
 | x = z    | 20        | flow  | /tmp/dflowtest4491306753443006638/Test8956901754875487242.c|
 | return x;| 21        | flow  | /tmp/dflowtest4491306753443006638/Test8956901754875487242.c|
 | RET      | 14        | flow  | /tmp/dflowtest4491306753443006638/Test8956901754875487242.c|
,  ____________________________________________________________________________________________
 | tracked  | lineNumber| method| file                                                       |
 |===========================================================================================|
 | a = p0   | 15        | flow  | /tmp/dflowtest4491306753443006638/Test8956901754875487242.c|
 | b=a      | 16        | flow  | /tmp/dflowtest4491306753443006638/Test8956901754875487242.c|
 | b + c    | 18        | flow  | /tmp/dflowtest4491306753443006638/Test8956901754875487242.c|
 | z = b + c| 18        | flow  | /tmp/dflowtest4491306753443006638/Test8956901754875487242.c|
 | x = z    | 20        | flow  | /tmp/dflowtest4491306753443006638/Test8956901754875487242.c|
 | return x;| 21        | flow  | /tmp/dflowtest4491306753443006638/Test8956901754875487242.c|
 | RET      | 14        | flow  | /tmp/dflowtest4491306753443006638/Test8956901754875487242.c|
,  ____________________________________________________________________________________________
 | tracked  | lineNumber| method| file                                                       |
 |===========================================================================================|
 | b=a      | 16        | flow  | /tmp/dflowtest4491306753443006638/Test8956901754875487242.c|
 | b + c    | 18        | flow  | /tmp/dflowtest4491306753443006638/Test8956901754875487242.c|
 | z = b + c| 18        | flow  | /tmp/dflowtest4491306753443006638/Test8956901754875487242.c|
 | x = z    | 20        | flow  | /tmp/dflowtest4491306753443006638/Test8956901754875487242.c|
 | return x;| 21        | flow  | /tmp/dflowtest4491306753443006638/Test8956901754875487242.c|
 | RET      | 14        | flow  | /tmp/dflowtest4491306753443006638/Test8956901754875487242.c|
,  ____________________________________________________________________________________________
 | tracked  | lineNumber| method| file                                                       |
 |===========================================================================================|
 | b + c    | 18        | flow  | /tmp/dflowtest4491306753443006638/Test8956901754875487242.c|
 | z = b + c| 18        | flow  | /tmp/dflowtest4491306753443006638/Test8956901754875487242.c|
 | x = z    | 20        | flow  | /tmp/dflowtest4491306753443006638/Test8956901754875487242.c|
 | return x;| 21        | flow  | /tmp/dflowtest4491306753443006638/Test8956901754875487242.c|
 | RET      | 14        | flow  | /tmp/dflowtest4491306753443006638/Test8956901754875487242.c|
,  ____________________________________________________________________________________________
 | tracked  | lineNumber| method| file                                                       |
 |===========================================================================================|
 | z = b + c| 18        | flow  | /tmp/dflowtest4491306753443006638/Test8956901754875487242.c|
 | x = z    | 20        | flow  | /tmp/dflowtest4491306753443006638/Test8956901754875487242.c|
 | return x;| 21        | flow  | /tmp/dflowtest4491306753443006638/Test8956901754875487242.c|
 | RET      | 14        | flow  | /tmp/dflowtest4491306753443006638/Test8956901754875487242.c|
,  ____________________________________________________________________________________________
 | tracked  | lineNumber| method| file                                                       |
 |===========================================================================================|
 | x = z    | 20        | flow  | /tmp/dflowtest4491306753443006638/Test8956901754875487242.c|
 | return x;| 21        | flow  | /tmp/dflowtest4491306753443006638/Test8956901754875487242.c|
 | RET      | 14        | flow  | /tmp/dflowtest4491306753443006638/Test8956901754875487242.c|
,  ____________________________________________________________________________________________
 | tracked  | lineNumber| method| file                                                       |
 |===========================================================================================|
 | return x;| 21        | flow  | /tmp/dflowtest4491306753443006638/Test8956901754875487242.c|
 | RET      | 14        | flow  | /tmp/dflowtest4491306753443006638/Test8956901754875487242.c|

    """
    }

    "find flows from z to method returns of flow" in {
      val source = cpg.identifier.name("z")
      val sink = cpg.method.name("flow").methodReturn
      sink.reachableByFlows(source).l.size shouldBe 2
    }
  }

}
