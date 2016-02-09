#!/usr/bin/python
# -*- coding: utf-8 -*-
"""
@author    : Shane Bradley
@contact   : sbradley@redhat.com
@version   : 0.1
@copyright : GPLv2


NEXT Features:
* Include DLM as one of the stat ones.
* Need to colorize tables in stats for ever other line.
* Finish remaining stat queries and stat tables like for pids.
* Finish glockstats and tableize() the output.

TODO:

* Warning on high demote_seconds, high waiter count, high DLM traffic.
* NEED OPTION: only_filesystems: -o To allow only showing results for certain filesystems instead of all of them
* NEED OPTION: Add ignore list items like ENDED, N/A from U lines see man page.
* NEED OPTION: To disable_call_trace so call trace not printed.
* Try creating charts for plotting like pygal to embed into web pages:
  http://www.pygal.org/en/latest/index.html or ggplot: http://ggplot.yhathq.com/
* Could i combine the data into 1 file. Take 8 glocktops, then write to 1 file
  with everything sorted by date to see what is happenign on all nodes at around
  same time. Do not think a way to group cause started at different times and
  takes different times to print data.


"""
import sys
import logging
import logging.handlers
import os
import os.path
from optparse import OptionParser, Option, SUPPRESS_HELP

from datetime import datetime
import re
import calendar

# For tableize()
import locale
locale.setlocale(locale.LC_NUMERIC, "")

# #####################################################################
# Global vars:
# #####################################################################
VERSION_NUMBER = "0.1-2"
MAIN_LOGGER_NAME = "%s" %(os.path.basename(sys.argv[0]))

# #####################################################################
# Classes
# #####################################################################
class GFS2FilesystemSnapshot():
    # A collection of glocks for a filesystem at a specific time.
    def __init__(self, filesystem_name, hostname, date_time, dlm_activity = None):
        self.__filesystem_name = filesystem_name
        self.__hostname = hostname
        self.__date_time = date_time
        self.__dlm_activity = dlm_activity

        self.__glocks = []
        self.__glocks_stats = None

    def __str__(self):
        dlm_activity = ""
        if (not self.get_dlm_activity() == None):
            dlm_activity = "(%s)" %(str(self.get_dlm_activity()))
        return "%s - %s %s %s" %(self.get_filesystem_name(), str(self.get_date_time()), self.get_hostname(), dlm_activity)

    def get_filesystem_name(self):
        return self.__filesystem_name

    def get_hostname(self):
        return self.__hostname

    def get_date_time(self):
        return self.__date_time

    def add_glock(self, glock):
        self.__glocks.append(glock)

    def get_glocks(self):
        return self.__glocks

    def find_glock(self, glock_type, glock_inode):
        glocks = []
        for glock in self.get_glocks():
            if (glock_inode == glock.get_inode()):
                if (0 < cmdline_opts.glock_type < 10):
                    if (glock_type == glock.get_type()):
                        glocks.append(glock)
                else:
                    glocks.append(glock)
        return glocks

    def get_glocks_stats(self):
        return self.__glocks_stats

    def add_glocks_stats(self, glocks_stats):
        self.__glocks_stats = glocks_stats

    def get_dlm_activity(self):
        return self.__dlm_activity

class DLM_Activity():
    def __init__(self, dlm_dirtbl_size, dlm_rsbtbl_size, dlm_lkbtbl_size, activity_count):
        self.__dlm_dirtbl_size = dlm_dirtbl_size
        self.__dlm_rsbtbl_size = dlm_rsbtbl_size
        self.__dlm_lkbtbl_size = dlm_lkbtbl_size
        self.__activity_count = activity_count

    def __str__(self):
        return "DLM: %d waiters with hash table sizes: %d/%d/%d" %(self.get_activity_count(),
                                                                   self.get_dlm_dirtbl_size(),
                                                                   self.get_dlm_rsbtbl_size(),
                                                                   self.get_dlm_lkbtbl_size())

    def get_dlm_dirtbl_size(self):
        return self.__dlm_dirtbl_size

    def get_dlm_rsbtbl_size(self):
        return self.__dlm_rsbtbl_size

    def get_dlm_lkbtbl_size(self):
        return self.__dlm_lkbtbl_size

    def get_activity_count(self):
        return self.__activity_count

class Glock():
    def __init__(self, gtype, inode, state, demote_time):
        self.__type = gtype
        self.__inode = inode
        self.__state = state
        self.__demote_time = demote_time

        # This list contains the holder and other waiting to be holders(waiters)
        self.__holders = []
        self.__glock_object = None

    def __str__(self):
        return "(%s/%s) state: %s | demote_time: %sms | hw count: %d" %(self.get_type(),
                                                                        self.get_inode(),
                                                                        self.get_state(),
                                                                        self.get_demote_time(),
                                                                        len(self.get_holders()))

    def get_type(self):
        return self.__type

    def get_inode(self):
        return self.__inode

    def get_state(self):
        return self.__state

    def get_demote_time(self):
        return self.__demote_time

    def add_holder(self, holder):
        self.__holders.append(holder)

    def get_holders(self):
        return self.__holders

    def add_glock_object(self, glock_object):
        self.__glock_object = glock_object

    def get_glock_object(self):
        return self.__glock_object

class GlockHolder:
    # The GlockHolder can be the holder of glock or waiter of glocks
    def __init__(self, text, state, flags, error, pid, comm):
        self.__text = text
        self.__state = state
        self.__flags = flags
        self.__error = error
        self.__pid = pid
        self.__comm = comm

    def __str__(self):
        return self.get_text()

    def get_text(self):
        return self.__text

    def get_state(self):
        return self.__state

    def get_flags(self):
        return self.__flags

    def get_error(self):
        return self.__error

    def get_pid(self):
        return self.__pid

    def get_command(self):
        return self.__command

class GlockObject():
    # The "I:" describes an inode associated with the lock, "R:" describes an
    # resource group associated with the glock, and "B:" describes a reservation
    # associated with a resource group.
    def __init__(self, text):
        self.__text = text

    def __str__(self):
        return self.get_text()

    def get_text(self):
        return self.__text

class GlocksStats():
    def __init__(self):
        # glocks    nondisk  inode    rgrp   iopen    flock  quota jrnl
        # S  Unlocked:       1        6       4       0       0     0    0       11
        # S    Locked:       2      370       6     124       0     0    1      504
        # S   Held EX:       0        2       0       0       0     0    1        3
        # S   Held SH:       1        1       0     123       0     0    0      125
        # S   Held DF:       0        0       0       0       0     0    0        0
        # S G Waiting:       0        1       0       0       0     0    0        1
        # S P Waiting:       0        1       0       0       0     0    0        1
        # S  DLM wait:       0        self.__glock_stats_category_order = ""
        self.__glocks_stats = []
        self.__glocks_stats_category_order = ["Unlocked","Locked", "Held EX", "Held SH", "Held DF", "G Waiting", "P Waiting", "DLM wait"]
    def __str__(self):
        rstring = ""
        for glock_stats in self.get_glocks_stats():
            rstring += "%s\n" %(str(glock_stats))
        return rstring.rstrip()

    def add_glock_stats(self, glock_stats):
        self.__glocks_stats.append(glock_stats)

    def get_glocks_stats(self):
        return self.__glocks_stats

class GlockStats():
    def __init__(self, glock_category, nondisk, inode, rgrp, iopen,
                 flock, quota, journal, total):
        self.__glock_category = glock_category
        self.__nondisk = nondisk
        self.__inode = inode
        self.__rgrp = rgrp
        self.__iopen = iopen
        self.__flock = flock
        self.__quota = quota
        self.__journal = journal
        self.__total = total

    def __str__(self):
        rstring =  "%s %s %s " %(self.get_glock_category(), self.get_nondisk(), self.get_inode())
        rstring += "%s %s %s " %(self.get_rgrp(), self.get_iopen(), self.get_flock())
        rstring += "%s %s %s " %(self.get_quota(), self.get_journal(), self.get_total())
        return rstring

    def get_glock_category(self):
        return self.__glock_category

    def get_nondisk(self):
        return self.__nondisk

    def get_inode(self):
        return self.__inode

    def get_rgrp(self):
        return self.__rgrp

    def get_iopen(self):
        return self.__iopen

    def get_flock(self):
        return self.__flock

    def get_quota(self):
        return self.__quota

    def get_journal(self):
        return self.__journal

    def get_total(self):
        return self.__total


# #####################################################################
# Parsers
# #####################################################################
def process_snapshot(gfs2_snapshot, snapshot_lines):
    # Process any remaining items
    if (not gfs2_snapshot == None):
        glock = None
        glocks_stats_lines = []
        for sline in snapshot_lines:
            if (sline.startswith("G")):
                glock = parse_glock(sline)
                gfs2_snapshot.add_glock(glock)
            elif (not glock == None and sline.startswith("H")):
                glock_holder = parse_glock_holder(sline)
                if (not glock_holder == None):
                    glock.add_holder(glock_holder)
            elif ((not glock == None) and
                  (sline.startswith("I") or
                   sline.startswith("R") or
                   sline.startswith("B"))):
                glock_object = GlockObject(sline)
                if (not glock_object == None):
                    glock.add_glock_object(glock_object)
            elif (sline.startswith("U")):
                # These lines represent glocktop's user interpretation of the
                # data, both glock and holder.  Lines that begin with (N/A:...)
                # can probably be ignored because they ought to be unimportant:
                # system files such as journals, etc.

                # Add certain lines to ignore list.
                continue
            elif (sline.startswith("C")):
                # These lines give you the call trace (call stack) of the process
                # that's either hold‐ing or waiting to hold the glock.
                continue
            elif (sline.startswith("S")):
                # These are not captured each time a filesystem is sampled.

                # These lines give you the summary of all glocks for this file
                # system: How many of each category are unlocked, locked, how
                # many are held in EX, SH, and DF, and how many are waiting. G
                # Waiting is how many glocks have waiters. P Waiting is how many
                # processes are waiting. Thus, you could have one glock that's
                # got ten processes waiting, or ten glocks that have ten
                # processes waiting.
                glocks_stats_lines.append(sline)
        glocks_stats = GlocksStats()
        for line in glocks_stats_lines:
            stat_map = parse_glock_stats(line)
            if (stat_map):
                glock_stats = GlockStats(stat_map.get("glock_category"),
                                         stat_map.get("nondisk"), stat_map.get("inode"),
                                         stat_map.get("rgrp"), stat_map.get("iopen"),
                                         stat_map.get("flock"), stat_map.get("quota"),
                                         stat_map.get("journal"), stat_map.get("total"))
                glocks_stats.add_glock_stats(glock_stats)
        if (glocks_stats.get_glocks_stats()):
            gfs2_snapshot.add_glocks_stats(glocks_stats)

def parse_glock_stats(line):
    try:
        stat_line = line.split("S ")[1].strip()
    except IndexError:
        return {}
    if ((stat_line.find("--") > 0) or (stat_line.find("Total") > 0)):
        return {}
    stats_map = {"glock_category":"", "nondisk":0, "inode":0, "rgrp":0, "iopen":0, "flock":0, "quota":0, "journal":0, "total":0}
    regex = "(?P<glock_category>Unlocked|Locked|Held EX|Held SH|Held DF|G Waiting|P Waiting):.*(?P<nondisk>\d+).*(?P<inode>\d+).*(?P<rgrp>\d+).*(?P<iopen>\d+).*(?P<flock>\d+).*(?P<quota>\d+).*(?P<journal>\d+).*(?P<total>\d+).*"
    rem = re.compile(regex)
    mo = rem.match(stat_line)
    if mo:
        return mo.groupdict()
    elif (stat_line.startswith("DLM wait")):
        split_stat_line = stat_line.split(":")
        return {"glock_category":split_stat_line[0].strip(), "nondisk":split_stat_line[1].strip(), "inode":0, "rgrp":0, "iopen":0, "flock":0, "quota":0, "journal":0, "total":split_stat_line[1].strip()}
    return {}

def parse_header(line):
    # @ nate_bob1       Mon Feb  1 15:04:11 2016  @host-050.virt.lab.msp.redhat.com

    days_regex = "(?P<day>%s)" % '|'.join(calendar.day_abbr[0:])
    months_regex = "(?P<month>%s)" % '|'.join(calendar.month_abbr[1:])
    dow_regex = "(?P<dow>\d{1,2})"
    time_regex = "(?P<time>\d{1,2}:\d\d:\d\d)"
    year_regex = "(?P<year>\d{4})"
    hostname_regex = "@(?P<hostname>.*)"
    regex = "^@ (?P<filesystem>\w+)\s+%s\s%s\s*%s\s%s\s%s\s\s%s" %(days_regex, months_regex, dow_regex, time_regex, year_regex, hostname_regex)

    rem = re.compile(regex)
    mo = rem.match(line)
    if mo:
        date_time = datetime.strptime("%s %s %s %s" %(mo.group("month"), mo.group("dow"), mo.group("year"), mo.group("time")), "%b %d %Y %H:%M:%S")
        split_line = mo.group("hostname").strip().split("dlm:")
        hostname = split_line[0]
        # Check to see if DLM data is at end of string contained in hostname
        # group.

        # The [*  ] increasing * means increasing dlm activity. Each * represents a line in the dlm's *_waiters file
        # $ cat /sys/kernel/debug/dlm/<fs>_waiters
        # 100b0 1 5        1               1
        # It's basically a list of dlm resources that are hung up waiting for a comm response over the network.
        # - So no max ceiling, just a line count in the file.
        # - 1 lock could have multiple waiters(or lines).
        dlm_activity = None
        if (len(split_line) == 2):
            dlm_regex = "(?P<dlm_dirtbl_size>\d+)/(?P<dlm_rsbtbl_size>\d+)/(?P<dlm_lkbtbl_size>\d+)\s\[(?P<dlm_activity>\*+).*"
            rem_dlm = re.compile(dlm_regex)
            mo_dlm = rem_dlm.match(split_line[1].strip())
            if mo_dlm:
                dlm_activity = DLM_Activity(int(mo_dlm.group("dlm_dirtbl_size")), int(mo_dlm.group("dlm_rsbtbl_size")),
                                            int(mo_dlm.group("dlm_lkbtbl_size")), len(mo_dlm.group("dlm_activity")))

        return GFS2FilesystemSnapshot(mo.group("filesystem"), hostname, date_time, dlm_activity)
    return None

def parse_glock(line):
    # G:  s:EX n:2/6fd40 f:lDpfiIqo t:UN d:UN/71000 a:0 v:4 r:5 m:10 (unknowninode)
    regex = re.compile("^G:  s:(?P<state>\S+) n:(?P<type>\d)/(?P<inodeNumber>\S+)\s" + \
                        "f:(?P<flags>\S*)\st:(?P<target>\S+)\sd:(?P<demote_state>\S+)/" + \
                       "(?P<demote_time>\d+)( l:(?P<lvbs>\d+))?\sa:(?P<ails>\d+)" +\
                       "( v:(?P<v>\d+))?\sr:(?P<refs>\d+)( m:(?P<hold>\d+))\s" + \
                       "\((?P<glock_type>.*)\)")
    mo = regex.match(line)
    if mo:
        #return Glock(int(mo.group("type")), int(mo.group("inodeNumber"), 16), mo.group("state"), mo.group("demote_time"))
        return Glock(int(mo.group("type")), mo.group("inodeNumber"), mo.group("state"), mo.group("demote_time"))
    return None
    parse_glock = staticmethod(parse_glock)

def parse_glock_holder(line):
    regex = re.compile("^H: s:(\S+) f:(\S+) e:(\d+) p:(\d+) \[(\S+)\] (.+)")
    mo = regex.match(line)
    if mo:
        #     def __init__(self, text, state, flags, error, pid, comm):
        return GlockHolder(line, mo.group(1), mo.group(2), mo.group(3),
                           mo.group(4), mo.group(5))
        #return GFS2GlockHolder(line.strip().rstrip(),mo.group(1), mo.group(2),
        #                       mo.group(3), mo.group(4), mo.group(5), mo.group(6))
    message = "There was an error parsing this line: %s" %(line)
    logging.getLogger(MAIN_LOGGER_NAME).debug(message)
    return None
    parse_glock_holder = staticmethod(parse_glock_holder)

# #####################################################################
# Helper classes
# #####################################################################
class ColorizeConsoleText(object):
    """
    References from:
    - https://gist.github.com/Jossef/0ee20314577925b4027f and modified bit.
    - https://gist.github.com/arulrajnet/47ff2674ad9da6dcac00
    - http://misc.flogisoft.com/bash/tip_colors_and_formatting (colors)
    """

    def __init__(self, text, **user_styles):
        # Prevent int and string concat error.
        text = str(text)

        styles = {
            # styles
            'reset': '\033[0m',
            'bold': '\033[01m',
            'disabled': '\033[02m',
            'underline': '\033[04m',
            'reverse': '\033[07m',
            'strike_through': '\033[09m',
            'invisible': '\033[08m',
            # text colors
            'fg_black': '\033[30m',
            'fg_red': '\033[31m',
            'fg_green': '\033[32m',
            'fg_orange': '\033[33m',
            'fg_blue': '\033[34m',
            'fg_purple': '\033[35m',
            'fg_cyan': '\033[36m',
            'fg_light_grey': '\033[37m',
            'fg_dark_grey': '\033[90m',
            'fg_light_red': '\033[91m',
            'fg_light_green': '\033[92m',
            'fg_yellow': '\033[93m',
            'fg_light_blue': '\033[94m',
            'fg_pink': '\033[95m',
            'fg_light_cyan': '\033[96m',
            'fg_white': '\033[97m',
            'fg_default': '\033[99m',
            # background colors
            'bg_black': '\033[40m',
            'bg_red': '\033[41m',
            'bg_green': '\033[42m',
            'bg_orange': '\033[43m',
            'bg_blue': '\033[44m',
            'bg_purple': '\033[45m',
            'bg_cyan': '\033[46m',
            'bg_light_grey': '\033[47m'
        }

        self.color_text = ''
        for style in user_styles:
            try:
                self.color_text += styles[style]
            except KeyError:
                raise KeyError("ERROR: def color: parameter `{}` does not exist".format(style))

        self.color_text += text

    def __format__(self):
        return '\033[0m{}\033[0m'.format(self.color_text)

    @classmethod
    def red(clazz, text):
        cls = clazz(text, bold=True, fg_red=True)
        return cls.__format__()

    @classmethod
    def orange(clazz, text):
        cls = clazz(text, bold=True, fg_orange=True)
        return cls.__format__()

    @classmethod
    def green(clazz, text):
        cls = clazz(text, bold=True, fg_green=True)
        return cls.__format__()

    @classmethod
    def custom(clazz, text, **custom_styles):
        cls = clazz(text, **custom_styles)
        return cls.__format__()

# #####################################################################
# Helper File Functions
# ####################################################################
def tableize(table, header):
    if (not len(table) > 0):
        return ""

    colorize_header = []
    for item in header:
        colorize_header.append(ColorizeConsoleText.red(item))
    table.insert(0, header)

    def format_num(num):
        try:
            inum = int(num)
            return locale.format("%.*f", (0, inum), True)
        except (ValueError, TypeError):
            return str(num)

    def get_max_width(table, index):
        return max([len(format_num(row[index])) for row in table])
    col_paddings = []

    for i in range(len(table[0])):
        col_paddings.append(get_max_width(table, i))

    ftable = ""
    for row in table:
        # left col
        ftable += str(row[0].ljust(col_paddings[0] + 1))
        # rest of the cols
        for i in range(1, len(row)):
            col = format_num(row[i]).rjust(col_paddings[i] + 2)
            ftable += str(col)
        ftable += "\n"

    return ftable

def write_to_file(path_to_filename, data, append_to_file=True, create_file=False):
    [parent_dir, filename] = os.path.split(path_to_filename)
    if (os.path.isfile(path_to_filename) or (os.path.isdir(parent_dir) and create_file)):
        try:
            file_mode = "w"
            if (append_to_file):
                file_mode = "a"
            fout = open(path_to_filename, file_mode)
            fout.write(data + "\n")
            fout.close()
            return True
        except UnicodeEncodeError, e:
            message = "There was a unicode encode error writing to the file: %s." %(path_to_filename)
            logging.getLogger(MAIN_LOGGER_NAME).error(message)
            return False
        except IOError:
            message = "There was an error writing to the file: %s." %(path_to_filename)
            logging.getLogger(MAIN_LOGGER_NAME).error(message)
            return False
    return False

def mkdirs(path_to_dir):
    if (os.path.isdir(path_to_dir)):
        return True
    elif ((not os.access(path_to_dir, os.F_OK)) and (len(path_to_dir) > 0)):
        try:
            os.makedirs(path_to_dir)
        except (OSError, os.error):
            message = "Could not create the directory: %s." %(path_to_dir)
            logging.getLogger(MAIN_LOGGER_NAME).error(message)
            return False
        except (IOError, os.error):
            message = "Could not create the directory with the path: %s." %(path_to_dir)
            logging.getLogger(MAIN_LOGGER_NAME).error(message)
            return False
    return os.path.isdir(path_to_dir)

def get_data_from_file(path_to_filename) :
    if (len(path_to_filename) > 0) :
        try:
            fin = open(path_to_filename, "r")
            data = fin.readlines()
            fin.close()
            mod_data = []
            for line in data:
                mod_data.append(line.strip())
            return mod_data
        except (IOError, os.error):
            message = "An error occured reading the file: %s." %(path_to_filename)
            logging.getLogger(MAIN_LOGGER_NAME).error(message)
    return []

# ##############################################################################
# Get user selected options
# ##############################################################################
def __get_options(version) :
    cmd_parser = OptionParserExtended(version)
    cmd_parser.add_option("-d", "--debug",
                         action="store_true",
                         dest="enableDebugLogging",
                         help="enables debug logging",
                         default=False)
    cmd_parser.add_option("-q", "--quiet",
                         action="store_true",
                         dest="disableLoggingToConsole",
                         help="disables logging to console",
                         default=False)
    cmd_parser.add_option("-y", "--no_ask",
                        action="store_true",
                         dest="disableQuestions",
                         help="disables all questions and assumes yes",
                         default=False)
    cmd_parser.add_option("-p", "--path_to_filename",
                         action="store",
                         dest="path_to_src_file",
                         help="the path to the filename that will be parsed",
                         type="string",
                         metavar="<input filename>",
                         default="")
    cmd_parser.add_option("-m", "--minimum_waiter_count",
                          action="store",
                          dest="minimum_waiter_count",
                          help="the minimum number of waiters for a glock",
                          type="int",
                          metavar="<minimum waiter count>",
                          default=1)
    cmd_parser.add_option("-W", "--disable_show_waiters",
                          action="store_true",
                          dest="disable_show_waiters",
                          help="the waiters for the glocks are not displayed in the output",
                          default=False)
    cmd_parser.add_option("-S", "--disable_stats",
                          action="store_true",
                          dest="disable_stats",
                          help="do not print stats",
                          default=False)
    #cmd_parser.add_option("-C", "--disable_call_trace",
    #                      action="store_true",
    #                      dest="disable_call_trace",
    #                      help="do not print call traces for holder/waiters",
    #                      default=False)
    cmd_parser.add_option("-g", "--find_glock",
                          action="store",
                          dest="glock_inode",
                          help="a glock hexadecimal number to search for",
                          type="string",
                          metavar="0x<glock number>",
                          default="")
    cmd_parser.add_option("-G", "--find_glock_type",
                          action="store",
                          dest="glock_type",
                          help="a glock type to search for (requires glock number (-g))",
                          type="int",
                          metavar="<glock type>",
                          default=None)

 # Get the options and return the result.
    (cmdLine_opts, cmdLine_args) = cmd_parser.parse_args()
    return (cmdLine_opts, cmdLine_args)

# ##############################################################################
# OptParse classes for commandline options
# ##############################################################################
class OptionParserExtended(OptionParser):
    def __init__(self, version) :
        self.__command_name = os.path.basename(sys.argv[0])
        OptionParser.__init__(self, option_class=ExtendOption,
                              version="%s %s\n" %(self.__command_name, version),
                              description="%s \n"%(self.__command_name))

    def print_help(self):
        self.print_version()
        examples_message = "\n"
        OptionParser.print_help(self)
        #print examples_message

class ExtendOption (Option):
    ACTIONS = Option.ACTIONS + ("extend",)
    STORE_ACTIONS = Option.STORE_ACTIONS + ("extend",)
    TYPED_ACTIONS = Option.TYPED_ACTIONS + ("extend",)

    def take_action(self, action, dest, opt, value, values, parser):
        if (action == "extend") :
            valueList = []
            try:
                for v in value.split(","):
                    # Need to add code for dealing with paths if there is option for paths.
                    newValue = value.strip().rstrip()
                    if (len(newValue) > 0):
                        valueList.append(newValue)
            except:
                pass
            else:
                values.ensure_value(dest, []).extend(valueList)
        else:
            Option.take_action(self, action, dest, opt, value, values, parser)

# ###############################################################################
# Main Function
# ###############################################################################
if __name__ == "__main__":
    try:
        # #######################################################################
        # Get the options from the commandline.
        # #######################################################################
        (cmdline_opts, cmdline_args) = __get_options(VERSION_NUMBER)
        # #######################################################################
        # Setup the logger and create config directory
        # #######################################################################
        # Create the logger
        logLevel = logging.INFO
        logger = logging.getLogger(MAIN_LOGGER_NAME)
        logger.setLevel(logLevel)
        # Create a new status function and level.
        logging.STATUS = logging.INFO + 2
        logging.addLevelName(logging.STATUS, "STATUS")

        # Log to main system logger that script has started then close the
        # handler before the other handlers are created.
        sysLogHandler = logging.handlers.SysLogHandler(address = '/dev/log')
        logger.addHandler(sysLogHandler)
        logger.info("The script has started running.")
        logger.removeHandler(sysLogHandler)

        # Create a function for the STATUS_LEVEL since not defined by python. This
        # means you can call it like the other predefined message
        # functions. Example: logging.getLogger("loggerName").status(message)
        setattr(logger, "status", lambda *args: logger.log(logging.STATUS, *args))
        stream_handler = logging.StreamHandler()
        stream_handler.setLevel(logLevel)
        stream_handler.setFormatter(logging.Formatter("%(levelname)s %(message)s"))
        logger.addHandler(stream_handler)

        # #######################################################################
        # Set the logging levels.
        # #######################################################################
        if ((cmdline_opts.enableDebugLogging) and (not cmdline_opts.disableLoggingToConsole)):
            logging.getLogger(MAIN_LOGGER_NAME).setLevel(logging.DEBUG)
            stream_handler.setLevel(logging.DEBUG)
            message = "Debugging has been enabled."
            logging.getLogger(MAIN_LOGGER_NAME).debug(message)
        if (cmdline_opts.disableLoggingToConsole):
            stream_handler.setLevel(logging.CRITICAL)


        # #######################################################################
        # Validate input
        # #######################################################################
        if (not cmdline_opts.path_to_src_file):
            logging.getLogger(MAIN_LOGGER_NAME).error("A path to a file (-p) to be analyzed is required.")
            sys.exit(1)
        if (not os.path.exists(cmdline_opts.path_to_src_file)):
            message ="The file does not exist: %s" %(cmdline_opts.path_to_src_file)
            logging.getLogger(MAIN_LOGGER_NAME).error(message)
            sys.exit(1)
        if (not cmdline_opts.glock_type == None):
            if (not (0 < cmdline_opts.glock_type < 10)):
                logging.getLogger(MAIN_LOGGER_NAME).error("The glock type (-G) must be an integer between 1 - 9.")
                sys.exit(1)
        if (not cmdline_opts.minimum_waiter_count > 0):
            logging.getLogger(MAIN_LOGGER_NAME).error("The minimum holder count for a glock (-m) must be a positive integer.")
            sys.exit(1)

        glock_inode = ""
        if (cmdline_opts.glock_inode):
            try:
                if (cmdline_opts.glock_inode.startswith("0x")):
                    int("%s" %(cmdline_opts.glock_inode), 16)
                else:
                    int("0x%s" %(cmdline_opts.glock_inode), 16)
            except ValueError:
                logging.getLogger(MAIN_LOGGER_NAME).error("The glock number (-g) must be a hexadecimal number.")
                sys.exit(1)
            except TypeError:
                logging.getLogger(MAIN_LOGGER_NAME).error("The glock number (-g) must be a hexadecimal number.")
                sys.exit(1)

        # #######################################################################
        # Run main
        # #######################################################################
        message ="The file will be analyzed: %s" %(cmdline_opts.path_to_src_file)
        logging.getLogger(MAIN_LOGGER_NAME).debug(message)
        # Get the data as a list of lines
        lines = get_data_from_file(cmdline_opts.path_to_src_file)

        #All the snapshots for all the filesystems.
        snapshots = []
        # The glock that will have a container for all the lines associated with
        # the glock.
        gfs2_snapshot = None
        # The lines that are related to this snapshot of the
        # filesystem. Including glocks, waiters, etc.
        snapshot_lines = []
        for line in lines:
            # @, G, H, I, R, B, U, C, S
            if ((line.startswith("@")) or (not len(line) > 0)):
                if (not gfs2_snapshot == None):
                    # Process any previous snapshot lines before starting a
                    # new one. All the glocks, holder/waiters, etc.
                    process_snapshot(gfs2_snapshot, snapshot_lines)
                    snapshots.append(gfs2_snapshot)
                # Process the new snapshot
                gfs2_snapshot = parse_header(line)
                snapshot_lines = []
            else:
                snapshot_lines.append(line)
        # Process any remaining items
        if (not gfs2_snapshot == None):
            process_snapshot(gfs2_snapshot, snapshot_lines)
            snapshots.append(gfs2_snapshot)

        # #######################################################################
        # Analyze the data
        # #######################################################################

        # Print summary of data analyzed
        summary = ""
        for snapshot in snapshots:
            current_summary = ""
            glocks = []
            if (cmdline_opts.glock_inode):
                # Find particular glocks.
                glocks = snapshot.find_glock(cmdline_opts.glock_type, cmdline_opts.glock_inode.replace("0x", ""))
            else:
                glocks = snapshot.get_glocks()
            for glock in glocks:
                glock_holders = glock.get_holders()
                if (len(glock_holders) >= cmdline_opts.minimum_waiter_count):
                    current_summary += "  %s\n" %(glock)
                    if (not cmdline_opts.disable_show_waiters):
                        for holder in glock_holders:
                            current_summary += "    %s\n" %(holder)
                        if (not glock.get_glock_object() == None):
                            current_summary += "    %s\n" %(glock.get_glock_object())
            if (current_summary):
                summary += "%s\n%s\n" %(ColorizeConsoleText.red(str(snapshot)), current_summary)
        print summary

        # Print stats
        if (not cmdline_opts.disable_stats):
            #
            # MAYBE NEED a GLOCK STAT OBEJECT to hold: hostname, filesname, date, glock, holder/waiter count, pids, demote_time.
            # Stuff like keeping up with glock-filesystem might get tricky and eventually parsing of multiple glocktop on multiple nodes.

            # * pid -> glocks with that pid | count
            # * glock -> pids
            # * peak for highest number of holder/waiters for each glock

            # Build structure so that filesystem, glocks stats can be analyzed
            filesystem_count = {}
            glock_count = {}
            glock_high_demote_seconds = {}

            # In some instances the unique key will be
            # "filesystem_name-glock_type/glock_inode". For example:
            # gfs2payroll-4/42ff2. Then for printing the filesystem and glock
            # info could be parsed out.
            for snapshot in snapshots:
                filesystem_name = snapshot.get_filesystem_name()
                if (filesystem_count.has_key(filesystem_name)):
                    filesystem_count[filesystem_name] = filesystem_count.get(filesystem_name) + 1
                else:
                    filesystem_count[filesystem_name] = 1
                for glock in snapshot.get_glocks():
                    glock_type_inode = "%s-%s/%s" %(filesystem_name, glock.get_type(), glock.get_inode())
                    if (glock_count.has_key(glock_type_inode)):
                        glock_count[glock_type_inode] = glock_count.get(glock_type_inode) + 1
                    else:
                        glock_count[glock_type_inode] = 1
                    demote_time = int(glock.get_demote_time())
                    if (demote_time > 0):
                        if (glock_high_demote_seconds.has_key(glock_type_inode)):
                            c_demote_time = glock_high_demote_seconds.get(glock_type_inode)
                            c_demote_time += " %d" %(demote_time)
                            glock_high_demote_seconds[glock_type_inode] = c_demote_time
                        else:
                            glock_high_demote_seconds[glock_type_inode] = "%s" %(demote_time)

            # Print filesystem stats
            table = []
            for key in filesystem_count.keys():
                table.append([key, filesystem_count.get(key)])
            ftable = tableize(table, ["Filesystem", "Snapshots"])
            if (len(ftable) > 0):
                print ftable

            # Print glock stats
            table = []
            from operator import itemgetter
            for pair in sorted(glock_count.items(), key=itemgetter(1), reverse=True):
                if (pair[1] > 1):
                    table.append([pair[0].rsplit("-")[0], pair[0].rsplit("-")[1], pair[1]])
            ftable = tableize(table, ["Filesystem Name", "Glock Type/Glocks Inode", "Total Holder/Waiter Count"])
            if (len(ftable) > 0):
                print ftable

            # Glock + filesystem with high demote seconds.
            table = []
            for key in glock_high_demote_seconds.keys():
                demote_seconds = glock_high_demote_seconds.get(key).split()
                index = 0
                current_fs_name = key.rsplit("-")[0]
                current_glock =  key.rsplit("-")[1]
                current_demo_seconds = ""
                for index in range(0, len(demote_seconds)):
                    if (((index % 7) == 0) and (not index == 0)):
                        table.append([current_fs_name, current_glock, current_demo_seconds])
                        current_fs_name = "-"
                        current_glock = "-"
                        current_demo_seconds = demote_seconds[index]
                    else:
                        current_demo_seconds += " %s" %(demote_seconds[index])

            ftable = tableize(table, ["Filesystem Name","Glock Type/Glocks Inode", "High Demote Seconds That Occurred (in ms)"])
            if (len(ftable) > 0):
                print ftable

            # DEBUGGING PRINT STATS
            #for snapshot in snapshots:
            #    glocks_stats = snapshot.get_glocks_stats()
            #    if (not glocks_stats == None):
            #        print snapshot.get_filesystem_name()
            #        print glocks_stats
            #        print
            print "GLOCKS STATS PRINT COMMENTED OUT FOR NOW."

    except KeyboardInterrupt:
        print ""
        message =  "This script will exit since control-c was executed by end user."
        logging.getLogger(MAIN_LOGGER_NAME).error(message)
        sys.exit(1)
    # #######################################################################
    # Exit the application with zero exit code since we cleanly exited.
    # #######################################################################
    sys.exit()
