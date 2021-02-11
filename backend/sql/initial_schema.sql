create table project
(
    project_id  serial not null primary key,
    name        text   not null unique,
    description text   null
);

create table test_methodology
(
    test_methodology_id serial        not null primary key,
    name                text          not null unique,
    description         text          null,
    confluence_link     varchar(1000) null
);

-- Corresponds to an EXCEL file
create table experiment
(
    experiment_id  serial                   not null primary key,
    project_id     int                      not null,
    methodology_id int                      not null,
    -- This is input from the user
    name           text                     null,
    meta           json                     null,
    -- This is fallback name to show if name is not specified
    raw_file_name  text                     not null,
    upload_date    timestamp with time zone not null,

    constraint belongs_to_project
        foreign key (project_id) references project (project_id)
            on update no action
            on delete cascade,

    constraint has_methodology
        foreign key (methodology_id) references test_methodology (test_methodology_id)
            -- Methodology_id is immutable
            on update no action
            -- Prevent removing methodology if there is an experiment having such methodology
            on delete no action
);

create table measurement
(
    measurement_id serial           not null primary key,
    experiment_id  int              not null,
    -- Either compound id or name
    compound_id    text             not null,
    concentration  double precision not null,
    signal         double precision not null,

    constraint belongs_to_experiment
        foreign key (experiment_id) references experiment (experiment_id)
            on delete cascade
            on update cascade
);

create table analysis_method
(
    analysis_method_id serial not null primary key,
    description        text   null,
    -- Analysis-specific parameters
    parameters         json   null
);

create table analysis
(
    analysis_id        serial not null primary key,
    analysis_method_id int    not null,
    experiment_id      int    not null,
    compound_id        int    not null,
    is_suspicious      bool   not null,
    -- Maybe JSON better?
    result             json   not null,

    constraint determined_by_analysis_method
        foreign key (analysis_method_id) references analysis_method (analysis_method_id)
            on delete cascade
            on update cascade,

    constraint such_experiment_exists
        foreign key (experiment_id) references experiment (experiment_id)
            on delete cascade
            on update cascade

    -- TODO do we want to make sure that there is at least one measurement for (experiment_id, compound_id) in
    -- measurement table?
);

create table analysis_removed_measurements
(
    analysis_id    int not null,
    measurement_id int not null,
    primary key (analysis_id, measurement_id),

    constraint belongs_to_analysis
        foreign key (analysis_id) references analysis (analysis_id)
            on delete cascade
            on update cascade,

    constraint such_measurement_exists
        foreign key (measurement_id) references measurement (measurement_id)
            on delete cascade
            on update cascade
);
