#!/bin/bash

echo "Navigate your browser to http://localhost:8090/"
erl -noshell -pa ./out/production/tic-tac-toe -s game_logic -s inets -config my_server

