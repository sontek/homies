#!/usr/bin/env bash

QUEUEDIR="$HOME/.msmtpqueue"

if [ ! -f "$QUEUEDIR" ]
then
    mkdir "$QUEUEDIR"
fi

LOCKFILE="$QUEUEDIR/.lock"
MAXWAIT=120

OPTIONS=$@

# wait for a lock that another instance has set
WAIT=0
while [ -e "$LOCKFILE" -a "$WAIT" -lt "$MAXWAIT" ]; do
	sleep 1
	WAIT="`expr "$WAIT" + 1`"
done
if [ -e "$LOCKFILE" ]; then
	echo "Cannot use $QUEUEDIR: waited $MAXWAIT seconds for"
	echo "lockfile $LOCKFILE to vanish, giving up."
	echo "If you are sure that no other instance of this script is"
	echo "running, then delete the lock file."
	exit 1
fi

# change into $QUEUEDIR 
cd "$QUEUEDIR" || exit 1

# check for empty queuedir
if [ "`echo *.mail`" = '*.mail' ]; then
	echo "No mails in $QUEUEDIR"
	exit 0
fi

# lock the $QUEUEDIR
touch "$LOCKFILE" || exit 1

# process all mails
for MAILFILE in *.mail; do
	MSMTPFILE="`echo $MAILFILE | sed -e 's/mail/msmtp/'`"
	echo "*** Sending $MAILFILE to `sed -e 's/^.*-- \(.*$\)/\1/' $MSMTPFILE` ..."
	if [ ! -f "$MSMTPFILE" ]; then
		echo "No corresponding file $MSMTPFILE found"
		echo "FAILURE"
		continue
	fi
	msmtp $OPTIONS `cat "$MSMTPFILE"` < "$MAILFILE"
	if [ $? -eq 0 ]; then
		rm "$MAILFILE" "$MSMTPFILE"
		echo "$MAILFILE sent successfully"
	else
		echo "FAILURE"
	fi
done

# remove the lock
rm -f "$LOCKFILE"

exit 0
