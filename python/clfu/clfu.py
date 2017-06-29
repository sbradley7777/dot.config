#!/usr/bin/env python
# This script queries commandlinefu.com for command examples. 
# https://gist.github.com/tuxfight3r/f5543df35a5099bd3451
import urllib2
import base64
import json
import os
import sys
import re

def sort_by_match(command):
    bestmatch = re.compile(r' ')
    search = bestmatch.sub('+', command)
    b64_encoded = base64.b64encode(search)
    url = "http://www.commandlinefu.com/commands/matching/" + search + "/" + b64_encoded + "/json"
    request = urllib2.Request(url, headers={ 'User-Agent': 'Mozilla/5.0' })
    response = json.load(urllib2.urlopen(request))
    for c in response:
        print "-" * 60
        print '#',c['summary']
        print c['command']

# ###############################################################################
# Main Function
# ###############################################################################
if __name__ == "__main__":
    command = ""
    if (len(sys.argv) > 1):
        command = sys.argv[1]
    else:
        command = raw_input("Please enter a search command: ")

    sort_by_match(command)
