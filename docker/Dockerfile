FROM r-base:3.2.2
COPY files/asreml.lic /root/

RUN apt-get update && apt-get install -y --no-install-recommends git libc6-dev libssl-dev
RUN apt-get install -y --no-install-recommends --no-upgrade libcurl4-gnutls-dev libssh2-1-dev
COPY files/Rprofile.site /etc/R/

RUN R -e 'install.packages("devtools")'
#moved asremlPlus install to the top, not to redo it every time code updates. 
RUN R -e 'devtools::install_version("asremlPlus", version="2.0-12", dependencies=TRUE, upgrade = FALSE, repos=c("http://cran.r-project.org", "https://cran.science-at-scale.io"))'

ARG PROJECT=RCB4Cloud

ADD ${PROJECT}.tgz /tmp/

RUN R -e 'devtools::install("/tmp/RCB4Cloud", dependencies=TRUE, upgrade = FALSE, repos=c("http://cran.r-project.org", "https://cran.science-at-scale.io"))'

RUN rm -rf '/tmp/${PROJECT}'
