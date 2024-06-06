#!/bin/bash

set -euo pipefail

# Build the project into a fat-jar
scala-cli package --assembly --preamble=false . -o main.jar -f

mkdir -p META-INF/services/

# Extract the specific file from the ZIP archive
unzip -p main.jar META-INF/services/org.flywaydb.core.extensibility.Plugin > META-INF/services/org.flywaydb.core.extensibility.Plugin.org

# Modify the file using sed
sed 's/PublishingConfigurationExtensionorg.flywaydb.database.cockroachdb.CockroachDBDatabaseType/PublishingConfigurationExtension\
org.flywaydb.database.cockroachdb.CockroachDBDatabaseType/' META-INF/services/org.flywaydb.core.extensibility.Plugin.org > META-INF/services/org.flywaydb.core.extensibility.Plugin

rm META-INF/services/org.flywaydb.core.extensibility.Plugin.org

# Replace the original file in the ZIP archive with the modified version
zip -u main.jar META-INF/services/org.flywaydb.core.extensibility.Plugin

# Clean up temporary files
rm -r META-INF

# Rename back to just main for Dockerfile
mv main.jar main

# Build the Docker image
docker buildx build --platform linux/amd64,linux/arm64 -t ghcr.io/lbialy/scala.today:$1 .

# Push the Docker image to GitHub Container Registry
docker push ghcr.io/lbialy/scala.today:$1


