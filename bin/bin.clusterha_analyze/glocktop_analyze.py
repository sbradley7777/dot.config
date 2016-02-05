#!/usr/bin/python
"""

@author    : Shane Bradley
@contact   : sbradley@redhat.com
@version   : 0.1
@copyright : GPLv2


TODO:

* Need to catch DLM on the filesystem line.
* Need table formatter
* warning on high demote_seconds, high waiter count, high DLM traffic.

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
VERSION_NUMBER = "0.1-1"
MAIN_LOGGER_NAME = "%s" %(os.path.basename(sys.argv[0]))

# #####################################################################
# Classes
# #####################################################################
class GFS2FilesystemSnapshot():
    # A collection of glocks for a filesystem at a specific time.
    def __init__(self, filesystem_name, hostname, date_time):
        self.__filesystem_name = filesystem_name
        self.__hostname = hostname
        self.__date_time = date_time

        self.__glocks = []

    def __str__(self):
        return "%s - %s %s" %(self.get_filesystem_name(), str(self.get_date_time()), self.get_hostname())

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

    def get_current_holder(self):
        # Return None or empty string if none have flag "h" and all are waiters
        # with "w" flag.
        return

    def add_glock_object(self, glock_object):
        self.__glock_object = glock_object

    def get_glock_object(self):
        return self.__glock_object

class GlockWaiter():
    # The GlockWaiter can be the holder of glock or waiter of glocks
    def __init__(self, line):
        self.__line = line

    def __str__(self):
        return self.__line

class GlockObject():
    # The "I:" describes an inode associated with the lock, "R:" describes an
    # resource group associated with the glock, and "B:" describes a reservation
    # associated with a resource group.
    def __init__(self, line):
        self.__line = line

    def __str__(self):
        return self.__line


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
# Parsers
# #####################################################################
def parse_header(line):
    # @ nate_bob1       Mon Feb  1 15:04:11 2016  @host-050.virt.lab.msp.redhat.com

    days = "(?P<day>%s)" % '|'.join(calendar.day_abbr[0:])
    months = "(?P<month>%s)" % '|'.join(calendar.month_abbr[1:])
    dow = "(?P<dow>\d{1,2})"
    time = "(?P<time>\d{1,2}:\d\d:\d\d)"
    year = "(?P<year>\d{4})"
    hostname = "@(?P<hostname>.*)"
    regex = "^@ (?P<filesystem>\w+)\s+%s\s%s\s*%s\s%s\s%s\s\s%s" %(days, months, dow, time, year, hostname)

    rem = re.compile(regex)
    mo = rem.match(line)
    if mo:
        date_time = datetime.strptime("%s %s %s %s" %(mo.group("month"), mo.group("dow"), mo.group("year"), mo.group("time")), "%b %d %Y %H:%M:%S")
        return GFS2FilesystemSnapshot(mo.group("filesystem"),  mo.group("hostname"), date_time)
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

# #####################################################################
# Helper File Functions
# ####################################################################
def tableize(table, header):
    if (not len(table) > 0):
        return ""

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
            if ((line.startswith("@")) or (not len(line) > 0)):
                if (not gfs2_snapshot == None):
                    # Process any previous snapshot lines before starting a
                    # new one. All the glocks, holder/waiters, etc.
                    glock = None
                    for sline in snapshot_lines:
                        if (sline.startswith("G")):
                            glock = parse_glock(sline)
                            gfs2_snapshot.add_glock(glock)
                        elif (not glock == None and sline.startswith("H")):
                            glock.add_holder(GlockWaiter(sline))
                        elif ((not glock == None) and
                              (sline.startswith("I") or
                               sline.startswith("R") or
                               sline.startswith("B"))):
                            glock.add_glock_object(GlockObject(sline))
                        # Add the rest of the ones like I/R/* and any other ones.
                    snapshots.append(gfs2_snapshot)
                # Process the new snapshot
                gfs2_snapshot = parse_header(line)
                snapshot_lines = []
            elif (line.startswith("S")):
                # Skip summary lines for now.
                continue
            elif (line.startswith("U")):
                # Skip friendly lines for now.
                continue
            else:
                snapshot_lines.append(line)

        # Process any remaining items
        if (not gfs2_snapshot == None):
            glock = None
            for sline in snapshot_lines:
                if (sline.startswith("G")):
                    glock = parse_glock(sline)
                    gfs2_snapshot.add_glock(glock)
                if ((not glock == None) and sline.startswith("H")):
                    glock.add_holder(sline)
                # Add the rest of the ones like I/R/* and any other ones.
            snapshots.append(gfs2_snapshot)
        # The data has been processed and now will be analyzed.

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
        # * like the number of times that glock showed up, number of iterations of the filesystem, number of times a pid shows up
        # * peak for highest number of holder/waiters for each glock
        # * glock with higher than zero demote_seconds
        # * Need my table formatter.


        # Build structure so that filesystem, glocks stats can be analyzed
        filesystem_count = {}
        glock_count = {}
        glock_high_demote_seconds = {}
        for snapshot in snapshots:
            filesystem_name = snapshot.get_filesystem_name()
            if (filesystem_count.has_key(filesystem_name)):
                filesystem_count[filesystem_name] = filesystem_count.get(filesystem_name) + 1
            else:
                filesystem_count[filesystem_name] = 1
            for glock in snapshot.get_glocks():
                glock_type_inode = "%s/%s" %(glock.get_type(), glock.get_inode())
                if (glock_count.has_key(glock_type_inode)):
                    glock_count[glock_type_inode] = glock_count.get(glock_type_inode) + 1
                else:
                    glock_count[glock_type_inode] = 1
                demote_time = int(glock.get_demote_time())
                if (demote_time > 0):
                    if (glock_high_demote_seconds.has_key(glock_type_inode)):
                        c_demote_time = glock_high_demote_seconds.get(glock_type_inode)
                        c_demote_time += " %ds" %(demote_time)
                        glock_high_demote_seconds[glock_type_inode] = c_demote_time
                    else:
                        glock_high_demote_seconds[glock_type_inode] = "%ss" %(demote_time)


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
                table.append([pair[0], pair[1]])
        ftable = tableize(table, ["Glock Type/Glocks Inode", "Holder/Waiter Count"])
        if (len(ftable) > 0):
            print ftable

        # Glock + filesystem with high demote seconds.
        table = []
        for key in glock_high_demote_seconds.keys():
            table.append([key, glock_high_demote_seconds.get(key)])
        ftable = tableize(table, ["Glock Type/Glocks Inode", "High Demote Seconds that occurred"])
        if (len(ftable) > 0):
            print ftable

    except KeyboardInterrupt:
        print ""
        message =  "This script will exit since control-c was executed by end user."
        logging.getLogger(MAIN_LOGGER_NAME).error(message)
        sys.exit(1)
    # #######################################################################
    # Exit the application with zero exit code since we cleanly exited.
    # #######################################################################
    sys.exit()
