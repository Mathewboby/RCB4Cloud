FROM r-base:3.2.2
COPY files/asreml.lic /root/

COPY files/moncerts/ /usr/local/share/ca-certificates/extra/moncerts
COPY files/sources.list /etc/apt/

RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 04EE7237B7D453EC AA8E81B4331F7F50 648ACFD622F3D138 && \
    apt-get update && apt-get install -y --no-install-recommends binutils gnupg2 && \
    apt-get update && apt-get install -y ca-certificates && \
    update-ca-certificates && \
    apt-get update && \
    apt-get install -y --no-install-recommends \
    git libc6-dev libssl-dev libxml2-dev libcurl4-gnutls-dev libssh2-1-dev

COPY files/Rprofile.site /etc/R/

RUN R -e 'install.packages(c("glue", "withr" ))' && \
    R -e 'withr::with_makevars(c(CXX = "g++ -std=c++11"), install.packages("xml2", verbose = TRUE))' && \
    R -e 'install.packages("roxygen2")' && \
    R -e 'install.packages("igraph")' && \
    R -e 'install.packages("devtools")'
#moved asremlPlus install to the top, not to redo it every time code updates. 
RUN R -e 'devtools::install_version("asremlPlus", version="2.0-12", dependencies=TRUE, upgrade = FALSE, repos=c("http://cran.r-project.org", "https://cran.science-at-scale.io"))'

ARG PROJECT=RCB4Cloud

ADD ${PROJECT}.tgz /tmp/

RUN R -e 'devtools::install("/tmp/RCB4Cloud", dependencies=TRUE, upgrade = FALSE, repos=c("http://cran.r-project.org", "https://cran.science-at-scale.io"))'

RUN rm -rf '/tmp/${PROJECT}'
