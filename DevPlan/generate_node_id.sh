#!/bin/sh
uuidgen -N "$1" --namespace "@oid" --sha1 | tr -d '-' | cut -c1-16