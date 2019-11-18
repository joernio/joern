FROM ubuntu:latest                                                              
                                                                                
RUN apt-get update && apt-get install -y \                                         
        python3 \                                                                  
        openjdk-8-jdk \                                                         
        git \                                                                   
        gnupg                                                                  
RUN echo "deb https://dl.bintray.com/sbt/debian /" | tee -a /etc/apt/sources.list.d/sbt.list 
RUN apt-key adv --keyserver hkps://keyserver.ubuntu.com:443 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823 && apt-get update
RUN apt-get install -y sbt && git clone https://github.com/ShiftLeftSecurity/joern.git && cd joern && sbt stage
WORKDIR /joern  
