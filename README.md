<!--
   - SPDX-FileCopyrightText: 2019-2021 Serokell <https://serokell.io>
   -
   - SPDX-License-Identifier: LicenseRef-ReplaceMe
   -->

**Please read this file as a raw textual file, it has comments for people
who create a new repo from this template.**

This repository is a template that we use at Serokell to create new repositories.
See [GitHub documentation](https://docs.github.com/en/github/creating-cloning-and-archiving-repositories/creating-a-repository-from-a-template)
for more details about this feature.

If you have just created a new repo from this template, please start from here.
Here are some things to keep in mind:
* We have [a page in Notion](https://www.notion.so/serokell/Create-a-repository-9028c5c379364407b8b2019b69d2e64a) describing how to create a new repository.
* Make sure to read meta-comments (you won't see them in rendered markdown because they are comments, but you can see them in raw files) and remove them eventually.
You can check that all meta-comments are removed using `git grep '\[\/\/\]:'`.
* Since we have repos at GitHub and GitLab, we provide both [`.github/`](.github/) and [`.gitlab/`](.gitlab/) folders, one of them should be removed.
* The root [LICENSE](./LICENSE) file is present because it's treated specially by GitHub and GitLab.
Please put the right license there (or delete it if there is no license).
* In general, it's highly advised that you go over all files and check whether they make sense in your new repo.
The aforementioned Notion page should help you.
If you make a PR that adapts this template for your new project, reviewers of your PR can help you as well.
* Everything above this line, except for the copyright header, is supposed to be removed.

[//]: # (All comments like this one are meta-comments, they are supposed to be read carefully)
[//]: # (and removed when you finish filling up this template for the needs of your repo.)

[//]: # (This is a template of the README.md file in a repo called 'patak'.)
[//]: # (It is very tentative, just to help you start.)

[//]: # (Logo is absolutely optional)
# ![](./img/logo.png) Patak

[//]: # (Badges if appropriate)
[//]: # (When you start a new project, usually there won't be many badges initially.)
[//]: # (But later you can add more badges like: CI status, GitHub releases, presence in)
[//]: # (package repositories such as Hackage, etc.)
[//]: # (We include only a license badge as an example, usually it can be added from the beginning.)

[![License: MPL 2.0](https://img.shields.io/badge/License-MPL%202.0-brightgreen.svg)](https://opensource.org/licenses/MPL-2.0)

[//]: # (Describe Patak)
Patak is a modern tool which is good at doing X using Y because Z.

[//]: # (References to the beginning of README might be useful if the README is very big.)
[//]: # (If it's not, feel free to remove them.)
## Build Instructions [↑](#-patak)

Run `make` to build everything.

## Usage [↑](#-patak)

Launch `patak` executable and see how it does X.
If you pass `--impress` you will be impressed.
Pass `--help` to see all flags and options.

[//]: # (Only for projects which don't use GitHub issues)
## Issue Tracker [↑](#-patak)

We use [YouTrack](https://issues.serokell.io/issues/PAT) as our issue
tracker. You can login using your GitHub account to leave a comment or
create a new issue.

## For Contributors [↑](#-patak)

Please see [CONTRIBUTING.md](CONTRIBUTING.md) for more information.

## About Serokell [↑](#-patak)

Patak is maintained and funded with ❤️ by [Serokell](https://serokell.io/).
The names and logo for Serokell are trademark of Serokell OÜ.

We love open source software! See [our other projects](https://serokell.io/community?utm_source=github) or [hire us](https://serokell.io/hire-us?utm_source=github) to design, develop and grow your idea!

[//]: # (TODO: consider making https://www.notion.so/serokell/Awesome-Serokell-our-repositories-list-36412d7f9e704098a2bbb41ff889d52b public and adding this link here.)
