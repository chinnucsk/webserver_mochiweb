#!/bin/sh
cd `dirname $0`

export ERL_LIBS=$ERL_LIBS:../mochiweb/:../ecouch/

erl \
    -name webapp@localhost \
    -pa ./ebin -pa ./include \
    -s make all \
    -eval "web_server:start(8000)"