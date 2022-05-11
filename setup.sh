#!/bin/sh

 export DATABASE_URL="$(heroku config -a scm-daw | grep DATABASE_URL | awk '{ print $2 }')"
