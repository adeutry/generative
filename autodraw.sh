#!/bin/bash

ls draw-*.rkt | entr racket -t /_ &
ls *.png | entr open -g /_
