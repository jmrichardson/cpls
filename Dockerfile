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
RUN git clone https://github.com/jmrichardson/cPLS && echo 5

# Run on start
CMD /usr/bin/Rscript --vanilla /home/user/cPLS/cpls.R >> /home/user/cPLS/store/console.log 2>&1

# docker run -v /home/john/cpls:/home/user/cPLS/store cpls cp /home/user/cPLS/data/config.R /home/user/cPLS/store/
# docker run -v /home/john/cpls:/home/user/cPLS/store cpls cp /home/user/cPLS/data/user_name.acc /home/user/cPLS/store/
# docker run -v /home/john/cpls:/home/user/cPLS/store cpls
# docker run -v /home/john/cpls:/home/user/cPLS/store -it cpls bash

# docker build -t cpls --force-rm=true --rm=true .
