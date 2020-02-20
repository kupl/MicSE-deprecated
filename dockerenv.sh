#!/bin/bash

# This script run docker environment, and open bash shell for developer.
# So this script runs docker container with "-it --rm" option, and open bash shell.

DOCKER_IMAGE_NAME="MicSE"
DOCKER_IMAGE_TAGNAME="test"
DOCKER_IMAGE=$DOCKER_IMAGE_NAME:$DOCKER_IMAGE_TAGNAME
DOCKER_RUN_TEMPLATE="docker run -it --rm"   

### Build docker image. If you want to skip it, comment below.
sudo docker build -t $DOCKER_IMAGE .

if [[ $? -eq 0 ]];  # If docker build or variable-naming SUCCESS, enter.
then 
    sudo $DOCKER_RUN_TEMPLATE $DOCKER_IMAGE /bin/bash
else
    echo "dockerenv.sh: docker build failed."
fi
