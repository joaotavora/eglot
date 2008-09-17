#!/bin/sh
cd $1
nice -n 20 make START=$2 COUNT=$3 check
