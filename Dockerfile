FROM ubuntu:14.04

# Contact Information
MAINTAINER John Richardson contact@peerlendingserver.com

# Description Label
LABEL Description="Command Line Peer Lending Server cPLS"

# Install R and Required Packages
RUN sh -c 'echo "deb http://cran.rstudio.com/bin/linux/ubuntu trusty/" >> /etc/apt/sources.list'
RUN gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9
RUN gpg -a --export E084DAB9 | sudo apt-key add -
RUN apt-get update
RUN apt-get -y install r-base
RUN echo 'install.packages(c("RCurl", "jsonlite", "dplyr", "stringr", "lubridate", "log4r", "parallel", "plotrix", "base64", "ggplot2", "xtable", "gbm"), repos="http://cran.us.r-project.org", dependencies=TRUE)' > /tmp/packages.R \ && Rscript /tmp/packages.R

# Install Java
RUN apt-get install -y default-jre

# Install R User
ENV HOME /home/user
RUN useradd --create-home --home-dir $HOME user && chown -R user:user $HOME
WORKDIR $HOME
USER user

CMD ["R"]
