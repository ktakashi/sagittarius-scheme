#!/bin/bash

apt update
apt install --no-install-recommends --no-install-suggests -y \
    libgc-dev zlib1g-dev libffi-dev libffi-dev libssl-dev \
    cmake make curl gcc g++

