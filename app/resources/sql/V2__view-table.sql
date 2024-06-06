CREATE TABLE projects_with_latest_release (
    projectId SERIAL NOT NULL,
    organization VARCHAR(1024) NOT NULL,
    repository VARCHAR(1024) NOT NULL,
    groupId VARCHAR(1024) NOT NULL,
    lastVersion VARCHAR(256) NOT NULL,
    artifactName VARCHAR(1024) NOT NULL,
    project VARCHAR(1024) NOT NULL,
    releaseDate TIMESTAMPTZ,
    licenses text[],
    language text[],
    platform text[],
    UNIQUE (organization, repository, artifactName)
);