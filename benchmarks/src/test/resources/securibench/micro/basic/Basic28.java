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
   
   $Id: Basic28.java,v 1.4 2006/04/04 20:00:40 livshits Exp $
 */
package securibench.micro.basic;

import java.io.IOException;
import java.io.PrintWriter;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import securibench.micro.BasicTestCase;
import securibench.micro.MicroTestCase;

/**
 * @servlet description="complicated control flow"
 * @servlet vuln_count = "2"
 */
public class Basic28 extends BasicTestCase implements MicroTestCase {
    protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws IOException {
        String name = req.getParameter("name");
        boolean b[] = new boolean[3];
        PrintWriter writer = resp.getWriter();
        if (b[0]) {
            if (b[0]) {
                if (b[0]) {
                    if (b[0]) {
                        if (b[0]) {
                            if (b[0]) {
                                if (b[0]) {
                                    if (b[0]) {
                                        if (b[0]) {
                                        }
                                    } else {
                                    }
                                } else {
                                }
                                if (b[0]) {
                                }
                            } else {
                            }
                        } else {
                            if (b[0]) {
                                if (b[0]) {
                                    if (b[0]) {
                                    }
                                } else {
                                }
                            } else {
                            }
                        }
                    } else {
                        if (b[0]) {
                            if (b[0]) {
                                if (b[0]) {
                                }
                                writer.println(name);       /* BAD */
                            } else {
                            }
                        } else {
                        }
                    }
                } else {
                    if (b[0]) {
                        if (b[0]) {
                            if (b[0]) {
                            }
                        } else {
                        }
                    } else {
                    }
                }
            } else {
                if (b[0]) {
                    if (b[0]) {
                        if (b[0]) {
                            if (b[0]) {
                                if (b[0]) {
                                }
                            } else {
                            }
                        } else {
                            if (b[0]) {
                                if (b[0]) {
                                    if (b[0]) {
                                    }
                                } else {
                                }
                            } else {
                            }
                        }
                    } else {
                        if (b[0]) {
                            if (b[0]) {
                                if (b[0]) {
                                }
                            } else {
                            }
                        } else {
                        }
                    }
                } else {
                    if (b[0]) {
                        if (b[0]) {
                            if (b[0]) {
                            }
                        } else {
                        }
                    } else {
                        if (b[0]) {
                            if (b[0]) {
                                if (b[0]) {
                                }
                            } else {
                            }
                        } else {
                        }
                    }
                }
            }
        } else {
            if (b[0]) {
                if (b[0]) {
                    if (b[0]) {
                        writer.println(name);       /* BAD */
                    }
                } else {
                }
            } else {
            }
        }
    }

    public String getDescription() {
        return "complicated control flow";
    }

    public int getVulnerabilityCount() {
        return 2;
    }
}