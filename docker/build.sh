#!/bin/bash
set -e

PROJ_REPO="https://github.platforms.engineering/TADS/RCB4Cloud.git"
PROJECT="RCB4Cloud"
ENVIRONMENT=$1
DEVBRANCH="Phase2-ANOVA-dev"
MITCHDEVBRANCH="Phase2AnovaDevSummary"

NPIMAGE=docker-registry.science-at-scale.io/rcb:nonprod
PRODIMAGE=docker-registry.science-at-scale.io/rcb:prod
DEVIMAGE=docker-registry.science-at-scale.io/rcb:dev

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

    docker build -t ${PRODIMAGE} .
    docker push ${PRODIMAGE} 
    docker rmi ${PRODIMAGE}

elif [[ "${ENVIRONMENT}" == "dev" ]] ; then
    echo "Dev image"
    git checkout ${DEVBRANCH}
    git pull

    git clone --single-branch --branch ${DEVBRANCH} ${PROJ_REPO}
    tar --exclude 'Reference' -czf ${PROJECT}.tgz ${PROJECT}

    docker build -t ${DEVIMAGE} .
    docker push ${DEVIMAGE}
    docker rmi ${DEVIMAGE}

elif [[ "${ENVIRONMENT}" == "md" ]] ; then
    echo "Mitch Dev image"
    git clone --single-branch --branch ${MITCHDEVBRANCH} ${PROJ_REPO}
    tar --exclude 'Reference' -czf ${PROJECT}.tgz ${PROJECT}

    docker built -t docker-registry.science-at-scale.io/rcb:dev-mitch .
    docker push docker-registry.science-at-scale.io/rcb:dev-mitch
    docker rmi docker-registry.science-at-scale.io/rcb:dev-mitch

else 
    echo "Non-prod image"
    git clone --depth=1 ${PROJ_REPO}
    tar --exclude 'Reference' -czf ${PROJECT}.tgz ${PROJECT}

    docker build -t ${NPIMAGE} .
    docker push ${NPIMAGE}
    docker rmi ${NPIMAGE}
fi

if [[ -d ${PROJECT} ]] ; then
    rm -rf ${PROJECT}
fi
