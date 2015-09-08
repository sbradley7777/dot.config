#!/usr/bin/python
"""@name        : pms_query.py
@description : This script will perform various queries to the Plex Media Server.
@author      : Shane Bradley
@contact     :  shanebradley@gmail.com
@version     : 0.65
@copyright   : GPLv2

#######################################################################
Requirements:
#######################################################################
The script uses the plex API supplied by this project which will need to be
installed:

  - https://github.com/mjs7231/python-plexapi
  # sudo pip install plexapi

  This script uses this tvdb API suppied by this project which will need to be
  installed:
  - https://github.com/dbr/tvdb_api
  # sudo easy_install tvdb_api

#######################################################################
Configuration File
#######################################################################
Edit the configuration file to add in username and password.
  $ cat ~/.pms_queryconf
  [login]
  username = <login>
  password = <password>
  pms_name = <name of plex media server>

  [filenames]
  filename_extensions = mp4 m4v mkv
  filename_tags = pt1 pt2 pt3 1080p 720p
#######################################################################
TODO
#######################################################################
* Need to analyze (-a) TV show analyze code and analyze the full path like
  show_name(year)/season/episode-filename

* Add option to create a url to search like youtube or torrent for missing in
  details output.

* Need to add try/expect "requests.exceptions.Timeout" to any section doing PMS cause if
  host is asleep and takes time to spin up.

* Add examples to usage().

* Trim down colorize class after I save the output and make sure pep
  complaint. Save class to snippets first.

"""
from optparse import OptionParser, Option, SUPPRESS_HELP
import ConfigParser
import logging.handlers
import logging
import sys
import os
import os.path
import math
import locale
import re

try:
    from plexapi.myplex import MyPlexUser
    from plexapi.exceptions import NotFound
except ImportError:
    print "Error: There was an error importing the library \"plexapi\". The library is required to be installed."
    print "Info:  The library needs to be installed and is located here: https://github.com/mjs7231/python-plexapi"
    sys.exit(1)

try:
    import requests
    from requests.exceptions import ConnectionError
except ImportError:
    print "Error: There was an error importing the library \"ConnectionError\" from \"requests.exceptions\". The library is required to be installed."
    print "Info:  The library needs to be installed called \"requests\"."
    sys.exit(1)

try:
    import tvdb_api
    import tvdb_exceptions
except ImportError:
    print "Error: There was an error importing the library \"tvdb_api\". The library is required to be installed."
    print "Info:  The library needs to be installed and is located at: https://github.com/dbr/tvdb_api"
    sys.exit(1)

# #####################################################################
# Global vars:
# #####################################################################
VERSION_NUMBER = "0.1-1"
MAIN_LOGGER_NAME = "%s" %(os.path.basename(sys.argv[0]))
CONFIG_FILE = os.path.expanduser("~/.pms_query.conf")

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
            'fg_yellow': '\033[33m',
            'fg_blue': '\033[34m',
            'fg_purple': '\033[35m',
            'fg_cyan': '\033[36m',
            'fg_light_grey': '\033[37m',
            'fg_dark_grey': '\033[90m',
            'fg_light_red': '\033[91m',
            'fg_light_green': '\033[92m',
            'fg_light_yellow': '\033[93m',
            'fg_light_blue': '\033[94m',
            'fg_pink': '\033[95m',
            'fg_light_cyan': '\033[96m',
            'fg_white': '\033[97m',
            'fg_default': '\033[39m',
            # background colors
            'bg_black': '\033[40m',
            'bg_red': '\033[41m',
            'bg_green': '\033[42m',
            'bg_yellow': '\033[43m',
            'bg_blue': '\033[44m',
            'bg_purple': '\033[45m',
            'bg_cyan': '\033[46m',
            'bg_light_grey': '\033[47m',
            'bg_dark_grey': '\033[100m',
            'bg_light_red': '\033[101m',
            'bg_light_green': '\033[102m',
            'bg_light_yellow': '\033[103m',
            'bg_light_blue': '\033[104m',
            'bg_light_purple': '\033[105m',
            'bg_light_cyan': '\033[106m',
            'bg_white': '\033[107m',
            'bg_default': '\033[49m'
        }

        self.color_text = ''
        for style in user_styles:
            try:
                self.color_text += styles[style]
            except KeyError:
                raise KeyError('def color: parameter `{}` does not exist'.format(style))

        self.color_text += text

    def __format__(self):
        return '\033[0m{}\033[0m'.format(self.color_text)

    @classmethod
    def red(clazz, text):
        cls = clazz(text, bold=True, fg_red=True)
        return cls.__format__()

    @classmethod
    def bg_red(clazz, text):
        cls = clazz(text, bold=True, bg_red=True)
        return cls.__format__()

    @classmethod
    def yellow(clazz, text):
        cls = clazz(text, bold=True, fg_yellow=True)
        return cls.__format__()

    @classmethod
    def bg_yellow(clazz, text):
        cls = clazz(text, bold=True, bg_yellow=True)
        return cls.__format__()

    @classmethod
    def green(clazz, text):
        cls = clazz(text, bold=True, fg_green=True)
        return cls.__format__()

    @classmethod
    def bg_green(clazz, text):
        cls = clazz(text, bold=True, bg_green=True)
        return cls.__format__()

    @classmethod
    def light_green(clazz, text):
        cls = clazz(text, bold=False, fg_light_green=True)
        return cls.__format__()

    @classmethod
    def bg_light_green(clazz, text):
        cls = clazz(text, bold=False, bg_light_green=True)
        return cls.__format__()

    @classmethod
    def light_grey(clazz, text):
        cls = clazz(text, bold=False, fg_light_grey=True)
        return cls.__format__()

    @classmethod
    def bg_light_grey(clazz, text):
        cls = clazz(text, bold=False, bg_light_grey=True)
        return cls.__format__()

    @classmethod
    def custom(clazz, text, **custom_styles):
        cls = clazz(text, **custom_styles)
        return cls.__format__()

# ##############################################################################
# Helper functions
# ##############################################################################
def __humanize_bytes(size, unit_abbrev=""):
    units = ['B','KiB','MiB','GiB','TiB','PiB','EiB','ZiB','YiB']
    size = abs(size)
    if (size == 0):
        if (not len(unit_abbrev) > 0):
            unit_abbrev = "B"
        return "0%s" %(unit_abbrev)
    elif (len(unit_abbrev) > 0):
        index = 0
        for unit in units:
            if (unit_abbrev.lower() == unit.lower()):
                break
            index = index + 1
        p = float(index)
        return "%.2f%s" % (size/math.pow(1024,p),units[int(p)])
    else:
        p = math.floor(math.log(size, 2)/10)
        return "%.2f%s" % (size/math.pow(1024,p),units[int(p)])

def __format_item(item):
    import locale
    locale.setlocale(locale.LC_NUMERIC, "")
    try:
        return str(item)
    except UnicodeEncodeError:
        return item.encode("utf-8")

# ##############################################################################
# Helper functions for metadata
# ##############################################################################
def __print_table(rows, colorize=True):
    """
    Prints out a table using the data in `rows`, which is assumed to be a
    sequence of sequences with the 0th element being the header.
    https://gist.github.com/lonetwin/4721748
    """
    # Convert all values in rows to strings.
    if (len(rows) > 0):
        converted_rows_to_str = []
        for row in rows:
            current_row = []
            for item in row:
                current_row.append(__format_item(item))
            if (len(current_row) > 0):
                converted_rows_to_str.append(current_row)
        # Figure out each column widths which is max column size for all rows.
        widths = [ len(max(columns, key=len)) for columns in zip(*converted_rows_to_str) ]
        # Print seperator
        print('-+-'.join( '-' * width for width in widths))
        # Print the header
        header, data = converted_rows_to_str[0], converted_rows_to_str[1:]
        print(
            ' | '.join( format(title, "%ds" % width) for width, title in zip(widths, header) )
        )
        # Print seperator
        print('-+-'.join( '-' * width for width in widths))
        # Print the data
        count = 0
        for row in data:
            row_string = " | ".join(format(cdata, "%ds" % width) for width, cdata in zip(widths, row))
            if (not row_string.startswith("-")):
                count = count + 1
            if (( (count % 2) == 0) and (colorize == True)):
                row_string = ColorizeConsoleText.light_grey(row_string)
            print row_string

def __get_tvdb_tv_show(pms_tv_show):
    # Put tvdb query in its own function to verify information and get correct
    # tvshow that matches what is in PMS.
    tvdb_query = None
    tvdb_show = None
    try:
        tvdb_query = tvdb_api.Tvdb()
    except tvdb_exceptions.tvdb_error:
        logging.getLogger(MAIN_LOGGER_NAME).error("There was a timeout error connecting to tvdb.com for metadata on the show: \"%s\"" %(pms_tv_show.title))
        return tvdb_show
    # Do reverse split, in case () in show title.
    try:
        tvdb_show = tvdb_query[pms_tv_show.title.split(" (")[0].strip()]
    except tvdb_exceptions.tvdb_error:
        logging.getLogger(MAIN_LOGGER_NAME).error("There was an error connecting to tvdb.com for metadata on the show: \"%s\"" %(pms_tv_show.title))
        tvdb_show = None
    except tvdb_shownotfound:
        logging.getLogger(MAIN_LOGGER_NAME).error("There was no match for the tv show \"%s\" on tvdb.com." %(pms_tv_show.title))
        tvdb_show = None
    if (not tvdb_show == None):
        tvdb_match = False
        if (not tvdb_show.data.get("overview") == None):
            if (tvdb_show.data.get("overview").strip().lower() == pms_tv_show.summary.strip().lower()):
                return tvdb_show
        if (not tvdb_show.data.get("firstaired") == None):
            if (tvdb_show.data.get("firstaired").strip() == pms_tv_show.originallyAvailableAt.strftime("%Y-%m-%d").strip()):
                return tvdb_show
        try:
            tvdb_show = tvdb_query[pms_tv_show.title.strip()]
        except tvdb_exceptions.tvdb_error:
            logging.getLogger(MAIN_LOGGER_NAME).error("There was an error connecting to tvdb.com for metadata on the show: \"%s\"" %(pms_tv_show.title))
            tvdb_show = None
        except tvdb_shownotfound:
            logging.getLogger(MAIN_LOGGER_NAME).debug("There was no match for the tv show \"%s\" on tvdb.com." %(pms_tv_show.title))
            tvdb_show = None
        if (not tvdb_show == None):
            if (not tvdb_show.data.get("overview") == None):
                if (tvdb_show.data.get("overview").strip().lower() == pms_tv_show.summary.strip().lower()):
                    return tvdb_show
            if (not tvdb_show.data.get("firstaired") == None):
                if (tvdb_show.data.get("firstaired").strip() == pms_tv_show.originallyAvailableAt.strftime("%Y-%m-%d").strip()):
                    return tvdb_show
    # Possible that it is a match without an overview or firstaired date. Return whatever match we got.
    return tvdb_show

def __analyze_tv_show(pms_tv_show):
    logging.getLogger(MAIN_LOGGER_NAME).warning("The analyzing option is currently not working and the show: \"%s\" will not be analyzed." %(pms_tv_show.title)

def __print_tv_show(pms_tv_show, show_missing_details=False):
    skip_specials = True
    tvdb_show = __get_tvdb_tv_show(pms_tv_show)
    if (tvdb_show == None):
        logging.getLogger(MAIN_LOGGER_NAME).debug("There was no match for the tv show \"%s\" on \"tvdb\"." %(pms_tv_show.title))
    else:
        pms_tv_show_seasons_attributes = []
        pms_tv_show_missing_episodes = []
        try:
            for season in pms.library.get(pms_tv_show.title).seasons():
                has_missing_episodes = ""
                if (len(tvdb_show[int(season.index)]) == len(season.episodes())):
                    has_missing_episodes = "0"
                elif (len(tvdb_show[int(season.index)]) < len(season.episodes())):
                    has_missing_episodes = "The PMS episode count(%d) is higher than what is on TVDB(%d)." %(len(season.episodes()), len(tvdb_show[int(season.index)]))
                elif (len(tvdb_show[int(season.index)]) > len(season.episodes())):
                    has_missing_episodes = "%d missing episodes on PMS." %(len(tvdb_show[int(season.index)]) - len(season.episodes()))
                    if (show_missing_details):
                        # keys are the episodes numbers, find out which episdoes i have and remove from list of episodes
                        if (skip_specials and (season.index == "0" or season.title == "Specials")):
                            continue
                        pms_episodes = []
                        for episode in season.episodes():
                            pms_episodes.append(int(episode.index))
                        missing_episodes = list(set(tvdb_show[int(season.index)].keys()) - set(pms_episodes))
                        for episode_num in missing_episodes:
                            # Create tuple of season num and episode num to use
                            # to query tvdb for missing episodes.
                            pms_tv_show_missing_episodes.append((int(season.index), episode_num, ))
                pms_tv_show_seasons_attributes.append([season.title, len(season.episodes()), has_missing_episodes])
        except requests.exceptions.ConnectionError as e:
            logging.getLogger(MAIN_LOGGER_NAME).debug("The metadata for the seasons failed: %s" %(pms_tv_show.title))
            pms_tv_show_seasons_attributes.append(["?", "?", "?"])
        except tvdb_exceptions.tvdb_seasonnotfound:
            logging.getLogger(MAIN_LOGGER_NAME).debug("Could not find season %s for %s" %(season.index, pms_tv_show.title))
            pms_tv_show_seasons_attributes.append(["?", "?", "?"])
        except NotFound:
            logging.getLogger(MAIN_LOGGER_NAME).debug("Could not find season %s for %s" %(season.index, pms_tv_show.title))
            pms_tv_show_seasons_attributes.append(["?", "?", "?"])
        if (len(pms_tv_show_seasons_attributes) > 0):
            # Print details of episodes on PMS.
            try:
                print "%s [Seasons: %02d] [Episodes: %02d]" %(pms_tv_show.title, len(pms.library.get(pms_tv_show.title).seasons()), len(pms.library.get(pms_tv_show.title).episodes()))
            except NotFound:
                logging.getLogger(MAIN_LOGGER_NAME).debug("Could not find season %s for %s" %(season.index, pms_tv_show.title))
                print "%s [Seasons: ?] [Episodes: ?]" %(pms_tv_show.title)
            pms_tv_show_seasons_attributes.insert(0, ["Season Title", "PMS Episode Count", "Missing Episodes"])
            __print_table(pms_tv_show_seasons_attributes)
            # If enabled print details of episodes on tvdb.
            if (show_missing_details and (len(pms_tv_show_missing_episodes) > 0)):
                print
                print "%s: Missing Episodes" %(pms_tv_show.title)
                missing_episodes_details = []
                for missing_episodes in pms_tv_show_missing_episodes:
                    try:
                        missing_episodes_details.append([str(missing_episodes[0]), str(missing_episodes[1]),
                                                         tvdb_show[missing_episodes[0]][missing_episodes[1]]["firstaired"],
                                                         tvdb_show[missing_episodes[0]][missing_episodes[1]]["episodename"]])
                    except tvdb_exceptions.tvdb_episodenotfound:
                        logging.getLogger(MAIN_LOGGER_NAME).debug("Could not find season %s episode %s for %s" %(missing_episodes[0],
                                                                                                                 missing_episodes[1],
                                                                                                                 pms_tv_show.title))
                missing_episodes_details.insert(0, ["Season", "Missing Episode", "Date Aired", "Missing Episode Title"])
                __print_table(missing_episodes_details)
            print
            print "---------------------"
            print

# ##############################################################################
# Get user selected options
# ##############################################################################
def __getOptions(version) :
    """
    This function creates the OptionParser and returns commandline
    a tuple of the selected commandline options and commandline args.

    The cmdlineOpts which is the options user selected and cmdLineArgs
    is value passed and  not associated with an option.

    @return: A tuple of the selected commandline options and commandline args.
    @rtype: Tuple

    @param version: The version of the this script.
    @type version: String
    """
    cmdParser = OptionParserExtended(version)
    cmdParser.add_option("-d", "--debug",
                         action="store_true",
                         dest="enableDebugLogging",
                         help="enables debug logging",
                         default=False)
    cmdParser.add_option("-q", "--quiet",
                         action="store_true",
                         dest="disableLoggingToConsole",
                         help="disables logging to console",
                         default=False)
    cmdParser.add_option("-l", "--list",
                         action="store_true",
                         dest="list",
                         help="list sections in library",
                         default=False)
    cmdParser.add_option("-a", "--analyze",
                         action="store_true",
                         dest="analyze",
                         help="analyze the metadata and filename",
                         default=False)
    cmdParser.add_option("-s", "--section_name",
                         action="store",
                         dest="section_name",
                         help="name of the section",
                         type="string",
                         metavar="<section name>",
                         default="")
    cmdParser.add_option("-t", "--section_type",
                         action="store",
                         dest="section_type",
                         help="type of media for a section: movie or show",
                         type="string",
                         metavar="<type of media for section>",
                         default="")
    cmdParser.add_option("-T", "--tv_show_title",
                         action="store",
                         dest="tv_show_title",
                         help="title of the tv show",
                         type="string",
                         metavar="<title of tv show>",
                         default="")
    cmdParser.add_option("-M", "--show_missing_details",
                         action="store_true",
                         dest="show_missing_details",
                         help="show details for missing episodes for tv show seasons",
                         default=False)
    cmdParser.add_option("-c", "--container",
                         action="store",
                         dest="container",
                         help="container type of media file",
                         type="string",
                         metavar="<container>",
                         default="")
    (cmdLineOpts, cmdLineArgs) = cmdParser.parse_args()
    return (cmdLineOpts, cmdLineArgs)

# ##############################################################################
# OptParse classes for commandline options
# ##############################################################################
class OptionParserExtended(OptionParser):
    """
    This is the class that gets the command line options the end user
    selects.
    """
    def __init__(self, version) :
        """
        @param version: The version of the this script.
        @type version: String
        """
        self.__commandName = os.path.basename(sys.argv[0])
        versionMessage = "%s %s\n" %(self.__commandName, version)

        commandDescription  ="%s \n"%(self.__commandName)

        OptionParser.__init__(self, option_class=ExtendOption,
                              version=versionMessage,
                              description=commandDescription)

    def print_help(self):
        """
        Print examples at the bottom of the help message.
        """
        self.print_version()
        examplesMessage = "\n"
        OptionParser.print_help(self)
        #print examplesMessage

class ExtendOption (Option):
    """
    Allow to specify comma delimited list of entries for arrays
    and dictionaries.
    """
    ACTIONS = Option.ACTIONS + ("extend",)
    STORE_ACTIONS = Option.STORE_ACTIONS + ("extend",)
    TYPED_ACTIONS = Option.TYPED_ACTIONS + ("extend",)

    def take_action(self, action, dest, opt, value, values, parser):
        """
        This function is a wrapper to take certain options passed on command
        prompt and wrap them into an Array.

        @param action: The type of action that will be taken. For example:
        "store_true", "store_false", "extend".
        @type action: String
        @param dest: The name of the variable that will be used to store the
        option.
        @type dest: String/Boolean/Array
        @param opt: The option string that triggered the action.
        @type opt: String
        @param value: The value of opt(option) if it takes a
        value, if not then None.
        @type value:
        @param values: All the opt(options) in a dictionary.
        @type values: Dictionary
        @param parser: The option parser that was orginally called.
        @type parser: OptionParser
        """
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
    """
    When the script is executed then this code is ran. If there was files(not
    directories) created then 0 will be returned, else a 1 is returned.
    """
    try:
        # #######################################################################
        # Get the options from the commandline.
        # #######################################################################
        (cmdLineOpts, cmdLineArgs) = __getOptions(VERSION_NUMBER)
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

        # Create a function for the STATUS_LEVEL since not defined by python. This
        # means you can call it like the other predefined message
        # functions. Example: logging.getLogger("loggerName").status(message)
        setattr(logger, "status", lambda *args: logger.log(logging.STATUS, *args))
        streamHandler = logging.StreamHandler()
        streamHandler.setLevel(logLevel)
        streamHandler.setFormatter(logging.Formatter("%(levelname)s %(message)s"))
        logger.addHandler(streamHandler)

        # #######################################################################
        # Set the logging levels.
        # #######################################################################
        if ((cmdLineOpts.enableDebugLogging) and (not cmdLineOpts.disableLoggingToConsole)):
            logging.getLogger(MAIN_LOGGER_NAME).setLevel(logging.DEBUG)
            streamHandler.setLevel(logging.DEBUG)
            message = "Debugging has been enabled."
            logging.getLogger(MAIN_LOGGER_NAME).debug(message)
        if (cmdLineOpts.disableLoggingToConsole):
            streamHandler.setLevel(logging.CRITICAL)

        # #######################################################################
        # Get login and password for connnecting to pms
        # #######################################################################
        if (not os.path.exists(CONFIG_FILE)):
            message = "The configuration file does not exist that contains the login credentials for plex."
            logging.getLogger(MAIN_LOGGER_NAME).error(message)
            sys.exit(1)
        configParser = ConfigParser.RawConfigParser()
        configParser.read(CONFIG_FILE)
        try:
            username = configParser.get("login", "username").strip()
            password = configParser.get("login", "password").strip()
            pms_name = configParser.get("login", "pms_name").strip()
        except ConfigParser.NoOptionError:
            logging.getLogger(MAIN_LOGGER_NAME).error("There was an error parsing the configuration file: %s" %(CONFIG_FILE))
            sys.exit(1)
        # Optional configurations options.
        filename_extensions = ""
        filename_tags = ""
        try:
            filename_extensions = configParser.get("filenames", "filename_extensions").strip().split()
        except ConfigParser.NoOptionError:
            # These can be skipped if not set.
            pass
        try:
            filename_tags = configParser.get("filenames", "filename_tags").strip().split()
        except ConfigParser.NoOptionError:
            # These can be skipped if not set.
            pass
        if (not len(username) > 0):
            logging.getLogger(MAIN_LOGGER_NAME).error("Please specfic a username in the configuration file: %s" %(CONFIG_FILE))
            sys.exit(1)
        elif(not len(password) > 0):
            logging.getLogger(MAIN_LOGGER_NAME).error("Please specfic a password in the configuration file: %s" %(CONFIG_FILE))
            sys.exit(1)
        elif(not len(pms_name) > 0):
            logging.getLogger(MAIN_LOGGER_NAME).error("Please specfic a Plex Media Server name in the configuration file: %s" %(CONFIG_FILE))
            sys.exit(1)

        # #######################################################################
        # Connect to PMS
        # #######################################################################
        message = "Connecting to your Plex Media Server: %s." %(pms_name)
        logging.getLogger(MAIN_LOGGER_NAME).debug(message)
        try:
            plex_user = MyPlexUser.signin(username, password)
            pms = plex_user.getResource(pms_name).connect()
        except requests.exceptions.SSLError:
            logging.getLogger(MAIN_LOGGER_NAME).error("There was an error signing on to the pms server: %s." %(pms_name))
            sys.exit(1)

        # Get section name and search to see if it exists.
        found_section_name = False
        if (len(cmdLineOpts.section_name) > 0):
            for section in pms.library.sections():
                if (section.title == cmdLineOpts.section_name):
                    found_section_name = True
            if (not found_section_name):
                logging.getLogger(MAIN_LOGGER_NAME).error("The section name does not exist: %s" %(cmdLineOpts.section_name))
                sys.exit(1)
        else:
            found_section_name = True

        # #######################################################################
        # List sections
        # #######################################################################
        if (cmdLineOpts.list):
            index = 1;
            sections = []
            for section in pms.library.sections():
                sections.append([str(index), section.title, section.type])
                index = index +  1;
            sections.insert(0, ["-", "Section Name", "Section Type"])
            __print_table(sections)
            sys.exit()


        if (len(cmdLineOpts.section_type) > 0) and (len(cmdLineOpts.section_name) > 0):
            logging.getLogger(MAIN_LOGGER_NAME).error("The -t and -s options cannot be used at the same time")
        elif (not len(cmdLineOpts.section_type) > 0) and (not len(cmdLineOpts.section_name) > 0):
            logging.getLogger(MAIN_LOGGER_NAME).error("A value for the option -t or -s is required.")

        # #######################################################################
        # Print metadata to console
        # #######################################################################
        section_types = []
        for section in pms.library.sections():
            if (section.type not in section_types):
                section_types.append(section.type)

        # Just add analyze code to here, if analyze disabled then just print, if
        # analyze enable then analyze media and only output the tests that fail.
        if (cmdLineOpts.analyze):
            logging.getLogger(MAIN_LOGGER_NAME).info("Analzying is still work in progress.")
            for section in pms.library.sections():
                media_attributes = []
                if (section.type == "movie") and ((section.title == cmdLineOpts.section_name) or (section.type == cmdLineOpts.section_type)):
                    total_section_size = 0
                    for movie in section.all():
                        # Get file details about the file for this metadata.
                        for ipart in movie.iter_parts():
                            ipart_container = ipart.container
                            if ((cmdLineOpts.container == ipart_container) or (not len(cmdLineOpts.container) > 0)):
                                ipart_filename = os.path.basename(ipart.file)
                                # Need to finish breaking down to movie_title,
                                # year, tags, extenstion.
                                regex = "^(?P<movie_title>[a-zA-Z_0-9\-+',&]*)\(.*(?P<year>[0-9]{4})\)(?P<tags>.*)\.(?P<extension>[a-zA-Z0-9]{3})"
                                rem = re.compile(regex)
                                mo = rem.match(ipart_filename)
                                if (mo):
                                    try:
                                        pms_title = ""
                                        pms_year = ""
                                        pms_tags = ""
                                        pms_ext = ""
                                        if (not __format_item(movie.title).lower() == __format_item(mo.group("movie_title")).lower().replace("_", " ")):
                                            pms_title = __format_item(movie.title).lower()
                                        if (not str(__format_item(movie.year)).lower() == __format_item(mo.group("year")).lower()):
                                            pms_year = __format_item(movie.year)
                                        if (( not __format_item(mo.group("extension")).lower() in filename_extensions) and (len(filename_extensions) > 0)):
                                            pms_ext = __format_item(mo.group("extension"))
                                        # Tags should follow this sequence:
                                        # ptX, resolution(1080p,720p), movie version(EE, DC)
                                        tags = mo.group("tags").split("-")
                                        for tag in tags:
                                            if ((not tag in filename_tags) and (len(filename_tags) > 0)):
                                                pms_tags = "%s, %s" %(pms_tags, tag)
                                        pms_tags = pms_tags.strip(", ").strip()
                                        if ((len(pms_title) > 0) or (len(pms_year) > 0) or
                                            (len(pms_tags) > 0) or (len(pms_ext) > 0)):
                                            media_attributes.append([ipart_filename, pms_title, pms_year, pms_tags, pms_ext])
                                    except IndexError:
                                        media_attributes.append([ipart_filename, "?", "?", "?","?"])
                                        logging.getLogger(MAIN_LOGGER_NAME).debug("There was a parsing error for: %s" %(ipart_filename))
                                else:
                                    media_attributes.append([ipart_filename, "?", "?", "?","?"])
                                    logging.getLogger(MAIN_LOGGER_NAME).debug("There was a parsing error for: %s" %(ipart_filename))
                    if (len(media_attributes) > 0):
                        media_attributes.insert(0, ["filename", "PMS Movie Title", "PMS Year", "tags", "extension"])
                        __print_table(media_attributes)
                        print
                        print "- \"?\": Represents unknown because parsing error occurred."
                        print "- \"*\": Represents incorrect value."
                        print "- The value that is in PMS will be printed if value in filename does not match."
                        print "- All strings are represented in lower case."
                        print
                elif (section.type == "show") and ((section.title == cmdLineOpts.section_name) or (section.type == cmdLineOpts.section_type)):
                    for pms_tv_show in section.all():
                        if (not len(cmdLineOpts.tv_show_title) > 0):
                            __analyze_tv_show(pms_tv_show)
                        elif ((cmdLineOpts.tv_show_title.lower().strip() == pms_tv_show.title.strip().lower()) or
                              (cmdLineOpts.tv_show_title.lower().strip() == pms_tv_show.title.rsplit(" (")[0].strip().lower())):
                            __analyze_tv_show(pms_tv_show)

            # exit, should make stuff below incased in else.
            sys.exit()
        for section in pms.library.sections():
            media_attributes = []
            if (section.type == "movie") and ((section.title == cmdLineOpts.section_name) or (section.type == cmdLineOpts.section_type)):
                counter = 1
                total_section_size = 0
                for movie in section.all():
                    ccounter = counter
                    # Get file details about the file for this metadata.
                    for ipart in movie.iter_parts():
                        ipart_container = ipart.container
                        if ((cmdLineOpts.container == ipart_container) or (not len(cmdLineOpts.container) > 0)):
                            ipart_filename = os.path.basename(ipart.file)
                            ipart_size = __humanize_bytes(ipart.size, "GiB")
                            total_section_size = total_section_size + int(ipart.size)
                            media_attributes.append([str(ccounter), movie.title, str(movie.year), ipart_container, ipart_filename, ipart_size])
                            # Dont increase count but replace with dash.
                            ccounter = "-"
                    if (ccounter == "-"):
                        counter = counter + 1;
                if (len(media_attributes) > 0):
                    media_attributes.insert(0, ["%s" %(section.title), "Movie Name", "Year", "Container", "Filename", "Size"])
                    media_attributes.append(["-", "-", "-", "-", "-", "-"])
                    media_attributes.append(["-", "-", "-", "-", "Total Section Size:", __humanize_bytes(total_section_size, "GiB")])
                    __print_table(media_attributes)
                    print
            elif (section.type == "show") and ((section.title == cmdLineOpts.section_name) or (section.type == cmdLineOpts.section_type)):
                for pms_tv_show in section.all():
                    if (not len(cmdLineOpts.tv_show_title) > 0):
                        __print_tv_show(pms_tv_show, show_missing_details=cmdLineOpts.show_missing_details)
                    elif ((cmdLineOpts.tv_show_title.lower().strip() == pms_tv_show.title.strip().lower()) or
                          (cmdLineOpts.tv_show_title.lower().strip() == pms_tv_show.title.rsplit(" (")[0].strip().lower())):
                        # Allow for multiple tv shows with same name. For
                        # example BSG.org and BGS.2003. I assume that metadata
                        # is for the correct show. Running analyze should show
                        # if not.
                        __print_tv_show(pms_tv_show, show_missing_details=cmdLineOpts.show_missing_details)
    except KeyboardInterrupt:
        print ""
        message =  "This script will exit since control-c was executed by end user."
        logging.getLogger(MAIN_LOGGER_NAME).error(message)
        sys.exit(1)
    # #######################################################################
    # Exit the application with zero exit code since we cleanly exited.
    # #######################################################################
    sys.exit()
