#!/bin/bash
set -e

IMAGE_NAME="annotationgx.sif"
DEF_FILE="annotationgx.def"

sudo apptainer build ${IMAGE_NAME} ${DEF_FILE}