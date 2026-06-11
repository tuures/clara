#!/bin/sh

BASEDIR=$(dirname "$0")
RUNSBT_DIR="$BASEDIR/.runsbt"
LAUNCHER_JAR="$RUNSBT_DIR/sbt-launch.jar"
SBTVER="1.12.11"

# download sbt-launch.jar if it doesn't exist
if [ ! -f "$LAUNCHER_JAR" ]; then
  echo "Downloading sbt-launch-$SBTVER.jar..."
  mkdir -p "$RUNSBT_DIR"
  curl -f -L -o "$LAUNCHER_JAR" \
    "https://repo1.maven.org/maven2/org/scala-sbt/sbt-launch/$SBTVER/sbt-launch-$SBTVER.jar" \
    || { echo "Failed to download sbt-launch.jar"; exit 1; }
  echo "Download complete."
fi

java -jar "$LAUNCHER_JAR" "$@"
