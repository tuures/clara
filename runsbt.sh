#!/bin/sh

BASEDIR=$(dirname "$0")

java -jar "$BASEDIR/.runsbt/sbt-launch.jar" "$@"
