#!/usr/bin/env bash

read -p "Did you update Settings.elm? " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]
then
    cd build
    aws s3 cp . s3://dubchan.net/ --recursive
fi

