#!/bin/bash
IMAP_USERNAME=$1
IMAP_PASSWORD=$2
if [ -z "$IMAP_USERNAME" -o -z "$IMAP_PASSWORD" ]; then
  echo >&2 "Usage: $0 imap-username imap-password"
  exit 1
fi

cd `dirname $0`/..
while true; do
  echo "Waiting for new email..."
  bin/idle_until_new_email.rb $IMAP_USERNAME $IMAP_PASSWORD
  if [ "$?" == "1" ]; then
    bin/getmail
  fi
done
