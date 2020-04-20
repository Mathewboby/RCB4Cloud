#!/bin/bash
set -e

PROJ_REPO="https://github.platforms.engineering/TADS/RCB4Cloud.git"
PROJECT="RCB4Cloud"
ENVIRONMENT=$1
DEVBRANCH="Phase2-ANOVA-dev"

if [[ -d ${PROJECT} ]] ; then
    rm -rf ${PROJECT}
fi

if [[ -f ${PROJECT}.tgz ]] ; then
    echo "Removing ${PROJECT}.tgz"
    rm -rf "${PROJECT}.tgz"
fi

# build, tag & push

if [[ "${ENVIRONMENT}" == "prod" ]] ; then
    echo "PROD image"
    git clone --depth=1 ${PROJ_REPO}
    tar --exclude 'Reference' -czf ${PROJECT}.tgz ${PROJECT}

    docker build -t rcb .
    docker tag rcb:latest docker-registry.science-at-scale.io/rcb:prod
    docker push docker-registry.science-at-scale.io/rcb:prod
elif [[ "${ENVIRONMENT}" == "dev" ]] ; then
    echo "Dev image"
    git clone --single-branch --branch ${DEVBRANCH} ${PROJ_REPO}
    tar --exclude 'Reference' -czf ${PROJECT}.tgz ${PROJECT}

    docker build -t rcb .
    docker tag rcb:latest docker-registry.science-at-scale.io/rcb:dev
    docker push docker-registry.science-at-scale.io/rcb:dev

else 
    echo "Non-prod image"
    git clone --depth=1 ${PROJ_REPO}
    tar --exclude 'Reference' -czf ${PROJECT}.tgz ${PROJECT}

    docker build -t rcb .
    docker tag rcb:latest docker-registry.science-at-scale.io/rcb:nonprod
    docker push docker-registry.science-at-scale.io/rcb:nonprod
fi

if [[ -d ${PROJECT} ]] ; then
    rm -rf ${PROJECT}
fi
