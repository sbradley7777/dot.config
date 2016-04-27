#!/bin/bash
# Detect which version of SSL that can be connected to.
# Based on poodle.sh

host=${1:-127.0.0.1};
port=${2:-8084};
timeout_bin=`which timeout 2>/dev/null`;

for ssl_version in "ssl2" "ssl3" ; do
    out="`echo 'Q' | ${timeout_bin:+$timeout_bin 5} openssl s_client -$ssl_version -connect "${host}:${port}" 2>/dev/null`"
    if [ $? -eq 124 ]; then
	echo "ERROR: Timeout connecting to \"$host:$port\" to check SSL version: $ssl_version.";
    else
	connected=`echo "$out" | grep '^CONNECTED'`;
	if [ $? -eq 0 ]; then
	    proto=`echo "$out" | grep '^ *Protocol *:' | awk '{ print $3 }'`;
	    cipher=`echo "$out" | grep '^ *Cipher *:' | awk '{ print $3 }'`;
	    echo "$host:$port | SSL version: $proto Cipher: $cipher";
	    if [ "$cipher" = '0000'  -o  "$cipher" = '(NONE)' ]; then
		echo "  Connected with \""${ssl_version^^}"\" but no ciphers were found.";
	    else
		echo "  Connected with \""${ssl_version^^}"\" and using cipher: $cipher.";
	    fi
	else
	    echo "$host:$port | SSL version: ${ssl_version^^}";
	    echo "  Failed to establish a \""${ssl_version^^}"\" connection.";
	fi
    fi
done
exit;
