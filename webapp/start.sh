#!/bin/sh
cd `dirname $0`

export ERL_LIBS=../mochiweb/

erl \
    -name webapp@localhost \
    -pa ./ebin -pa ./include \
    -s make all \
    -eval "rest_server:start(8000)"