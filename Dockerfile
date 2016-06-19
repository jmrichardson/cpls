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
RUN apt-get update &&  apt-get -y install r-base default-jre git

# Install R packages
RUN echo 'install.packages(c("RCurl", "jsonlite", "dplyr", "stringr", "lubridate", "log4r", "parallel", "plotrix", "base64", "ggplot2", "xtable", "gbm"), repos="http://cran.us.r-project.org", dependencies=TRUE)' > /tmp/packages.R \ && Rscript /tmp/packages.R

# Install R User
ENV HOME /home/user/
RUN useradd --create-home --home-dir $HOME user && chown -R user:user $HOME
WORKDIR $HOME
USER user

# Clone GitHub cPLS repository
RUN git clone https://github.com/jmrichardson/cPLS



# CMD ["R"]
# /usr/local/bin/Rscript --vanilla /home/user/pls/order.R >> log/order.Rout 2>&1

# docker build -t cpls --force-rm=true --rm=true .
# docker build -t cpls --no-cache --force-rm --rm .
# docker run -it -u user cpls /bin/bash
