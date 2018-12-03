FROM rocker/geospatial:latest
MAINTAINER "Adam Mahood" adam.mahood@colorado.edu

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    awscli

RUN install2.r --error \ 
  assertthat \ 
  caret \
  caTools \
  doParallel \ 
  foreach \
  gdalUtils \
  ggthemes \ 
  httr \
  party \
  picante \
  randomForest \
  rasterVis \ 
  RCurl \ 
  ROCR \
  snowfall \ 
  tidyverse \ 
  viridis
