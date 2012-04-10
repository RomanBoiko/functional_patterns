#!/bin/bash

erlc -Wall proxy.erl

echo "=========OUT==========="

erl -noshell -s proxy
