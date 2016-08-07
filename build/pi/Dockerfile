FROM armv7/armhf-ubuntu
# FROM ubuntu:14.04

# Contact Information
MAINTAINER John Richardson contact@peerlendingserver.com

# Description Label
LABEL Description="Command Line Peer Lending Server"

# Install R and Required Packages
ENV DEBIAN_FRONTEND noninteractive
# RUN sh -c 'echo "deb http://cran.rstudio.com/bin/linux/ubuntu xenial/" >> /etc/apt/sources.list'
# RUN gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9
# RUN gpg -a --export E084DAB9 | apt-key add -

# Update and install ubunutu packages
RUN apt-get update && apt-get -y upgrade 
RUN apt-get -y install default-jre default-jdk git wget curl unzip ntp sudo openssl libssl-dev libcurl4-openssl-dev vim
RUN apt-get -y install r-base

# Install R packages
RUN echo 'install.packages(c("RCurl", "jsonlite", "dplyr", "stringr", "lubridate", "log4r", "parallel", "plotrix", "base64", "ggplot2", "xtable", "modeltools","mailR","tools","caret","xgboost"), repos="http://cran.us.r-project.org", dependencies=TRUE)' > /tmp/packages.R \ && Rscript /tmp/packages.R
RUN echo 'install.packages(c("quantmod","NMOF","snow","quantmod","shiny","plotly"), repos="http://cran.us.r-project.org", dependencies=TRUE)' > /tmp/packages.R \ && Rscript /tmp/packages.R

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
RUN git clone https://github.com/jmrichardson/cpls && echo 37
# RUN mkdir /home/user/cpls
# COPY . /home/user/cpls

USER root
RUN chown -R user:user $HOME
USER user

# Run on start
CMD /usr/bin/Rscript --vanilla /home/user/cpls/cpls.R >> /home/user/cpls/store/console.log 2>&1

#  
# mkdir cpls
# chmod 751 cpls
# docker run -u user:user -v /home/pi/cpls:/home/user/cpls/store cpls cp /home/user/cpls/data/config.R /home/user/cpls/store
# docker run -u user:user -v /home/pi/cpls:/home/user/cpls/store cpls cp /home/user/cpls/data/user_name.acc /home/user/cpls/store


# docker run -v /home/john/cpls:/home/user/cpls/store cpls /usr/bin/Rscript --vanilla /home/user/cpls/cpls.R runOnce
# docker run --restart=on-failure:10 -v /home/john/cpls:/home/user/cpls/store -d cpls
# docker inspect -f "{{ .RestartCount }}" b30cc9d2ddf4
# docker run -v /home/john/cpls:/home/user/cpls/store -it cpls bash

# docker build -t cpls .


# Run in debug mode so you can do traceback
# docker run -u user:user -v /home/pi/cpls:/home/user/cpls/store -it cpls bash
# cd cpls
# R
# source('cpls.R',echo=T)


