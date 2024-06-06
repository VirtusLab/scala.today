-- Create the projects table
CREATE TABLE projects (
    id SERIAL PRIMARY KEY,
    organization VARCHAR(1024) NOT NULL,
    repository VARCHAR(1024) NOT NULL,
    UNIQUE (organization, repository)
);

-- Create the artifacts table
CREATE TABLE artifacts (
    id SERIAL PRIMARY KEY,
    groupId VARCHAR(1024) NOT NULL,
    artifactId VARCHAR(1024) NOT NULL,
    version VARCHAR(256) NOT NULL,
    artifactName VARCHAR(1024) NOT NULL,
    project VARCHAR(1024) NOT NULL,
    projectFk INTEGER NOT NULL,
    releaseDate TIMESTAMPTZ,
    licenses text[],
    language VARCHAR(50),
    platform VARCHAR(50),
    FOREIGN KEY (projectFk) REFERENCES projects(id),
    UNIQUE (groupId, artifactId, version)
);