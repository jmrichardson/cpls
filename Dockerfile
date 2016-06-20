FROM ubuntu:14.04

# Contact Information
MAINTAINER John Richardson contact@peerlendingserver.com

# Description Label
LABEL Description="Command Line Peer Lending Server cPLS"

# Install R and Required Packages
RUN sh -c 'echo "deb http://cran.rstudio.com/bin/linux/ubuntu trusty/" >> /etc/apt/sources.list'
RUN gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9
RUN gpg -a --export E084DAB9 | sudo apt-key add -

# Update and install ubunutu packages
RUN apt-get update &&  apt-get -y install r-base default-jre git libcurl4-gnutls-dev libssl-dev 
# RUN apt-get -y install nano

# Install R packages
RUN echo 'install.packages(c("RCurl", "jsonlite", "dplyr", "stringr", "lubridate", "log4r", "parallel", "plotrix", "base64", "ggplot2", "xtable", "gbm"), repos="http://cran.us.r-project.org", dependencies=TRUE)' > /tmp/packages.R \ && Rscript /tmp/packages.R

# install.packages("RCurl",repos="http://cran.us.r-project.org", dependencies=TRUE)

# Install R User
RUN echo "user ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers
ENV HOME /home/user/
RUN useradd --create-home --home-dir $HOME user && chown -R user:user $HOME
RUN usermod -a -G sudo user
WORKDIR $HOME
USER user

# Clone GitHub cPLS repository
RUN git clone https://github.com/jmrichardson/cPLS && echo 4

# Create required directories
RUN mkdir /home/user/cPLS/logs/
RUN mkdir /home/user/cPLS/tmp/
RUN mkdir /home/user/cPLS/config/

# Run on start
CMD /usr/bin/Rscript --vanilla /home/user/cPLS/order.R >> /home/user/cPLS/logs/console.log 2>&1

# docker run -v /home/john/test:/home/user/cPLS/config cpls cp /home/user/cPLS/data/config.tpl /home/user/cPLS/config/
# docker run -v /home/john/test:/home/user/cPLS/config cpls cp /home/user/cPLS/data/account.tpl /home/user/cPLS/config/
# docker run -v /home/john/test:/home/user/cPLS/config cpls
# docker run -v /home/john/test:/home/user/cPLS/config -it cpls bash

# docker build -t cpls --force-rm=true --rm=true .
