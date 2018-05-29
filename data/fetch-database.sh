#!/bin/bash

curl -c /tmp/cookies "https://drive.google.com/uc?export=download&id=1pMaVMzax0VjF7WHhZzpCcWGCAL5NcCHP" > /tmp/intermezzo.html
curl -L -b /tmp/cookies "https://drive.google.com$(cat /tmp/intermezzo.html | grep -Po 'uc-download-link" [^>]* href="\K[^"]*' | sed 's/\&amp;/\&/g')" > local.db