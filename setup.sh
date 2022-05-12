#!/bin/sh

 export DATABASE_URL="$(heroku config | grep DATABASE_URL | awk '{ print $2 }')"
