#!/bin/bash

flip(){
  mv "../data/JSON/standard" "../data/JSON/standard-tmp"
  mv "../data/JSON/norm" "../data/JSON/standard"
}

unflip(){
  mv "../data/JSON/standard" "../data/JSON/norm"
  mv "../data/JSON/standard-tmp" "../data/JSON/standard" 
}

MODE=$1

if [ "$MODE" == "-u" ]; then
  unflip
else
  flip
fi