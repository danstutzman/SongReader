#!/bin/bash
ERROR=2 # pretend that we found new mail at the beginning
while [ 1 ]; do
  if [ "$ERROR" == "0" ]; then
    echo "Not checking mail because IDLE command timed out"
  elif [ "$ERROR" == "2" ]; then
    bin/getmail
    ERROR=$?
    if [ "$ERROR" != "0" ]; then
      echo 1>&2 "Exiting because error..."
      exit $ERROR
    fi

    ruby process_mail.rb
    ERROR=$?
    if [ "$ERROR" != "0" ]; then
      echo 1>&2 "Exiting because error..."
      exit $ERROR
    fi
  else
    echo 1>&2 "Exiting because error..."
    exit $ERROR
  fi

  echo "Waiting for new mail..."
  bin/idle_until_new_email.rb
  ERROR=$?
done
