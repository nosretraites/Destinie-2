FROM r-base:3.6.2

ENV LC_ALL C.UTF-8

# install dependencies
RUN apt update -yq && \
  apt install -y libssl-dev libcurl4-openssl-dev libxml2-dev

# install OpenJDK-8
RUN apt-get update && \
    apt-get install -y r-cran-rjava

# Fix certificate issues and install python
RUN apt-get update && \
    apt-get install ca-certificates-java && \
    apt-get clean && \
    update-ca-certificates -f


# Setup JAVA_HOME -- useful for docker commandline
ENV JAVA_HOME /usr/lib/jvm/default-java/

# install R dependencies
RUN Rscript -e "install.packages(c('ggplot2', 'tidyr', 'dplyr', 'openxlsx', 'xlsx','jsonlite'))"

COPY . /destinie2
WORKDIR /destinie2

# expose
VOLUME /destinie2/data

RUN R CMD INSTALL . --preclean --debug

RUN mkdir -p /var/log/destinie/files

RUN apt-get install -y python3 python3-pip
