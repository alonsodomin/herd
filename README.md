# Herd

[![CircleCI](https://circleci.com/gh/alonsodomin/herd.svg?style=svg)](https://circleci.com/gh/alonsodomin/herd)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Mergify Status](https://img.shields.io/endpoint.svg?url=https://gh.mergify.io/badges/alonsodomin/herd&style=flat)](https://mergify.io)

Distributed schema-driven commit log. The idea behind Herd is to make a scalable self-descriptive storage that allows fine grained traceability of each of its records.

## Building

First of all, ensure that your local setup meets the following requirements

* [Rocks DB](https://rocksdb.org/)

Then clone the repo ensuring that you recurse over the git submodules, and run run `make`.

```bash
git clone --recursive https://github.com/alonsodomin/herd
cd herd
make
```

## Status

The project right now is still in experimental/discovery phase. This first goal is to have a minimal client/server protocol defined.

## License

Herd
Copyright (C) 2019  A. Alonso Dominguez

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <http://www.gnu.org/licenses/>.
