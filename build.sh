#!/bin/bash

elm-app build
docker build -t terminal-resume .
