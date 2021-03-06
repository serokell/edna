-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- Ongoing project in a company.
-- Experiment data is grouped into projects, i. e. each experiment fully
-- belongs to a single project.
-- Moreover, all experiments from one experiment data file belong to one project.
create table if not exists project
(
    project_id    serial    not null primary key,
    name          text      not null unique,
    description   text      null,
    creation_date timestamp not null default now (),
    last_update   timestamp not null default now ()
);

-- Describes how signals were measured and what they denote (their semantics).
-- All experiments from one experiment data file have the same methodology.
create table if not exists test_methodology
(
    test_methodology_id serial        not null primary key,
    name                text          not null unique,
    description         text          null,
    confluence_link     varchar(1000) null
);

-- Something that reacts with compounds that scientists want to research.
-- The goal of their work is to check how a compound affects a target.
create table if not exists target
(
    target_id       serial      not null primary key,
    name            text        not null unique,
    addition_date   timestamp   not null default now()
);

create table if not exists compound
(
    compound_id     serial          not null primary key,
    name            text            not null unique,
    addition_date   timestamp       not null default now(),
    chemsoft_link   varchar(1000)   null,
    mde_link        varchar(1000)   null
);

-- Corresponds to an Excel file.
-- Here we store data that is shared by all experiments in one file,
-- namely methodology and project.
-- Additionally we store file name and binary contents because
-- it should be possible to download these files.
create table if not exists experiment_file
(
    experiment_file_id serial    not null primary key,
    project_id         int       not null,
    methodology_id     int       null,  -- normally not null, but methodology can be removed
    upload_date        timestamp not null default now(),
    -- Arbitrary metadata from the file
    meta               json      not null,
    -- Description provided from UI, it's like additional metadata
    -- that we store in a separate string
    description        text      not null,

    -- Name of the file
    name               text      not null,
    -- .xlsx blob
    contents           bytea     not null,

    constraint belongs_to_project
        foreign key (project_id) references project (project_id)
            -- Project id is immutable
            on update no action
            -- Deletion is currently not supported for projects
            on delete no action,

    constraint has_methodology
        foreign key (methodology_id) references test_methodology (test_methodology_id)
            -- Methodology id is immutable
            on update no action
            -- If the corresponding methodology is removed, we set it to NULL
            on delete set null
);

-- Subset of data from a single experiment data file.
-- It contains a list of experimental measurements for ONE compound and ONE target.

-- You can think about it as a list of concentrations and 1 or more signal values
-- for each concentration. Usually there are multiple repetitions,
-- so more than 1 value for each concentration.
create table if not exists experiment
(
    experiment_id       serial not null primary key,
    -- File from which this experiment was taken.
    -- Project, methodology and some other data are inherited from that file.
    experiment_file_id  int    not null,
    compound_id         int    not null,
    target_id           int    not null,

    constraint has_compound
        foreign key (compound_id) references compound (compound_id)
            -- Compound id is immutable
            on update no action
            -- Deletion is currently not supported for compounds
            on delete no action,

    constraint has_target
        foreign key (target_id) references target (target_id)
            -- Target id is immutable
            on update no action
            -- Deletion is currently not supported for targets
            on delete no action,

    constraint has_experiment_file
        foreign key (experiment_file_id) references experiment_file (experiment_file_id)
            -- Experiment file id is immutable
            on update no action
            -- Deletion is currently not supported for experiment files
            on delete no action
);

create table if not exists measurement
(
    measurement_id  serial          not null primary key,
    experiment_id   int             not null,
    concentration   double precision not null,
    signal          double precision not null,
    -- ↓ Whether it was explicitly marked as outlier in source data
    is_outlier      bool,

    constraint belongs_to_experiment
        foreign key (experiment_id) references experiment (experiment_id)
            on delete no action
            on update no action
);

create table if not exists analysis_method
(
    analysis_method_id serial not null primary key,
    description        text   null,
    -- Analysis-specific parameters
    parameters         json   not null
);

-- Currently we have only one hardcoded analysis method.
insert into analysis_method values (1, 'IC50', '[]'::json) on conflict do nothing;

-- Sub-experiment is an experiment with disabled points.
-- An experiment always has at least one sub-experiment of itself.
-- Each sub-experiment has a name.
-- We define analysis for sub-experiments.
-- For each sub-experiment we store information about applied analysis method
-- and analysis outcome (result).
-- If we use multiple analysis methods for one set of data, there will be
-- multiple sub-experiments.
-- Additionally, we specify whether a sub-experiment is suspicious
-- (something looks wrong in its data).
create table if not exists sub_experiment
(
    sub_experiment_id  serial not null primary key,
    analysis_method_id int    not null,
    name               text   not null,
    experiment_id      int    not null,
    is_suspicious      bool   not null,
    result             json   not null,

    constraint determined_by_analysis_method
        foreign key (analysis_method_id) references analysis_method (analysis_method_id)
            on delete no action
            on update no action,

    constraint such_experiment_exists
        foreign key (experiment_id) references experiment (experiment_id)
            on delete no action
            on update no action

    -- TODO do we want to make sure that there is at least one measurement for (experiment_id, compound_id) in
    -- measurement table?
);

-- Each experiment has exactly one primary sub-experiment.
-- We store this relation in a separate table to avoid circular references.
create table if not exists primary_sub_experiment
(
    experiment_id       int not null unique,
    sub_experiment_id   int not null unique,
    primary key (experiment_id, sub_experiment_id),

    constraint belongs_to_experiment
        foreign key (experiment_id) references experiment (experiment_id)
            -- Experiments can't be deleted.
            on delete no action
            on update no action,

    constraint belongs_to_subexperiment
        foreign key (sub_experiment_id) references sub_experiment (sub_experiment_id)
            -- Prohibit deleting primary sub-experiment.
            on delete no action
            on update no action
);

create table if not exists removed_measurements
(
    sub_experiment_id   int not null,
    measurement_id      int not null,
    primary key (sub_experiment_id, measurement_id),

    constraint belongs_to_subexperiment
        foreign key (sub_experiment_id) references sub_experiment (sub_experiment_id)
            on delete cascade
            on update no action,

    constraint such_measurement_exists
        foreign key (measurement_id) references measurement (measurement_id)
            on delete no action
            on update no action
);
