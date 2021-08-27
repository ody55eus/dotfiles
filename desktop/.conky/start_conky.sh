#!/bin/bash
killall conky
sleep 10
conky -c ~/.conky/conky.conf
