FROM ubuntu:14.04

# Contact Information
MAINTAINER John Richardson contact@peerlendingserver.com

# Description Label
LABEL Description="Command Line Peer Lending Server"

# Install R and Required Packages
RUN sh -c 'echo "deb http://cran.rstudio.com/bin/linux/ubuntu trusty/" >> /etc/apt/sources.list'
RUN gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9
RUN gpg -a --export E084DAB9 | apt-key add -

# Update and install ubunutu packages
RUN apt-get update && apt-get -y upgrade
RUN apt-get -y install r-base default-jre default-jdk git wget curl unzip ntp sudo
RUN apt-get -y install openssl libssl-dev libcurl4-openssl-dev

# Install R packages
RUN echo 'install.packages(c("RCurl", "jsonlite", "dplyr", "stringr", "lubridate", "log4r", "parallel", "plotrix", "base64", "ggplot2"), repos="http://cran.us.r-project.org", dependencies=TRUE)' > /tmp/packages.R \ && Rscript /tmp/packages.R
RUN echo 'install.packages(c("xtable","modeltools","mailR","tools","caret","xgboost","NMOF","snow","shiny","plotly"), repos="http://cran.us.r-project.org", dependencies=TRUE)' > /tmp/packages.R \ && Rscript /tmp/packages.R

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
RUN mkdir /home/user/cpls
COPY . /home/user/cpls
USER root
RUN chown -R user:user $HOME
USER user

# Run on start
CMD /usr/bin/Rscript --vanilla /home/user/cpls/cpls.R >> /home/user/cpls/store/console.log 2>&1

