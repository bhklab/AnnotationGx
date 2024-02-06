#!/bin/bash

export DOCKER_REPO="jjjermiah"
export DOCKER_IMAGE_NAME="annotationgx-r"
export DOCKER_TAG="0.1.1"

docker build -t $DOCKER_REPO/$DOCKER_IMAGE_NAME:$DOCKER_TAG -f build/annotationGx.Dockerfile .

# then push to dockerhub
docker push $DOCKER_REPO/$DOCKER_IMAGE_NAME:$DOCKER_TAG
