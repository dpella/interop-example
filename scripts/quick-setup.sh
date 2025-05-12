#!/bin/bash

# Build the docker image
docker build -t sql-interoperability-example .

# Run the examples from the docker container. Remove the container after it
# exits
docker run --rm sql-interoperability-example