#!/bin/bash
erl +K true +A 5 -pa ebin -pa deps/*/ebin -config proxima1 -s proxima -sname proxima1@localhost
