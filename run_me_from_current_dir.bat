echo off
echo Edit this file to set proper erl.exe path!
echo Navigate your browser to http://localhost:8090/
echo on
erl -noshell -pa ./out/production/tic-tac-toe -s game_logic -s inets -config my_server