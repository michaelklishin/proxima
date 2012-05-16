#!/bin/bash
erl +K true +A 5 -pa ebin -pa deps/*/ebin -config proxima2 -s proxima -sname proxima2@localhost
