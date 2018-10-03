#!/bin/bash

FILENAME=$1
echo $FILENAME | entr -s "racket -t $FILENAME && open pic.png"
