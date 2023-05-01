#!/usr/bin/env sh
cd ../build
aws s3 cp . s3://dubchan.net/ --recursive

