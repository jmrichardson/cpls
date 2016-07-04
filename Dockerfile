FROM ubuntu:14.04

# Contact Information
MAINTAINER John Richardson contact@peerlendingserver.com

# Description Label
LABEL Description="Command Line Peer Lending Server"

# Install R and Required Packages
RUN sh -c 'echo "deb http://cran.rstudio.com/bin/linux/ubuntu trusty/" >> /etc/apt/sources.list'
RUN gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9
RUN gpg -a --export E084DAB9 | sudo apt-key add -

# Update and install ubunutu packages
RUN apt-get update && apt-get -y upgrade && apt-get -y install r-base default-jre default-jdk git wget curl unzip ntp sudo \
openssl libssl-dev libcurl4-openssl-dev
# libcurl4-gnutls-dev

# Install R packages
RUN echo 'install.packages(c("RCurl", "jsonlite", "dplyr", "stringr", "lubridate", "log4r", "parallel", "plotrix", "base64", "ggplot2", "xtable", "gbm", "modeltools","mailR","tools"), repos="http://cran.us.r-project.org", dependencies=TRUE)' > /tmp/packages.R \ && Rscript /tmp/packages.R

# Set PDT timezone
ENV TIMEZONE "America/Los_Angeles"      
RUN echo $TIMEZONE > /etc/timezone                     
RUN cp /usr/share/zoneinfo/${TIMEZONE} /etc/localtime

# Install R User
RUN echo "user ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers
ENV HOME /home/user/
RUN useradd --create-home --home-dir $HOME user && chown -R user:user $HOME
RUN usermod -a -G sudo user
WORKDIR $HOME
USER user

# Clone GitHub cpls repository
RUN git clone https://github.com/jmrichardson/cpls && echo 28

# Run on start
CMD /usr/bin/Rscript --vanilla /home/user/cpls/cpls.R >> /home/user/cpls/store/console.log 2>&1


# docker run -v /home/john/cpls:/home/user/cpls/store cpls cp /home/user/cpls/data/config.R /home/user/cpls/store/
# docker run -v /home/john/cpls:/home/user/cpls/store cpls cp /home/user/cpls/data/user_name.acc /home/user/cpls/store/

# docker run -v /home/john/cpls:/home/user/cpls/store cpls /usr/bin/Rscript --vanilla /home/user/cpls/cpls.R runOnce
# docker run -v /home/john/cpls:/home/user/cpls/store -d cpls
# docker run -v /home/john/cpls:/home/user/cpls/store -it cpls bash

# docker build -t cpls --force-rm=true --rm=true .
