CREATE TABLE failed_main_view_artifacts(
    msg TEXT NOT NULL,
    project TEXT NOT NULL,
    artifactName TEXT NOT NULL,
    version TEXT NOT NULL,
    createdAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
