#!/bin/bash
set -e

PROJ_REPO="https://github.platforms.engineering/TADS/RCB4Cloud.git"
PROJECT="RCB4Cloud"
ENVIRONMENT=$1

if [ -d ${PROJECT} ] ; then 
    rm -rf ${PROJECT}
fi

git clone --depth=1 ${PROJ_REPO}
tar -czf ${PROJECT}.tgz ${PROJECT}

if [ -d ${PROJECT} ] ; then 
    rm -rf ${PROJECT}
fi

# build 
docker build -t rcb .

# tag & push

if [ "${ENVIRONMENT}" == "prod" ] ; then
    echo "PROD image"
    docker tag rcb:latest docker-registry.science-at-scale.io/rcb:prod
    docker push docker-registry.science-at-scale.io/rcb:prod
  else 
    echo "Non-prod image"
    docker tag rcb:latest docker-registry.science-at-scale.io/rcb:nonprod
    docker push docker-registry.science-at-scale.io/rcb:nonprod
fi
