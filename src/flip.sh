#!/bin/bash

flip(){
  mv "../data/JSON/standard" "../data/JSON/standard-tmp"
  mv "../data/JSON/norm" "../data/JSON/standard"
}

unflip(){
  mv "../data/JSON/standard" "../data/JSON/norm"
  mv "../data/JSON/standard-tmp" "../data/JSON/standard" 
}

control_file=".fliprc"
OPT=$1

if ! [ -e $control_file ] || [ "$(cat $control_file)" == "unfliped" ]; then
  flip
  echo "fliped" > $control_file
else
  unflip
  echo "unfliped" > $control_file
fi