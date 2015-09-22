#!/usr/bin/python
"""@name        : pms_query.py

@description : This script will perform various queries to the Plex Media
               Server(PMS). This script can do basic analyzing of filenames
               vs. the pms title, displaying missing episodes for a tv show that
               has a season on PMS, and listing of media on PMS.
@author      : Shane Bradley
@contact     :  shanebradley@gmail.com
@version     : 0.5-1
@copyright   : GPLv2

#######################################################################
Requirements:
#######################################################################
The script uses the plex API supplied by this project which will need to be
installed: https://github.com/mjs7231/python-plexapi
  # sudo pip install plexapi

This script uses this tvdb API suppied by this project which will need to be
installed: https://github.com/dbr/tvdb_api
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

* Need to handle cases when some config options are not configured like
  "filename*" that are optional. Note: not even sure using those config options
  yet. Check later.

* Need to add code to fix (-f) filenames. Probably needs to be do u want to
  change to X, hit yes or can change automatically with default
  suggestion. (Y/N/M) and M for modify manually.

* Need to analyze (-a) TV show analyze code and analyze the full path like
  show_name(year)/season/episode-filename

* Add option to create a url to search like youtube or torrent for missing in
  details output.

* Need to add try/expect "requests.exceptions.Timeout" to any section doing PMS cause if
  host is asleep and takes time to spin up.

* Add examples to usage().

* What about naming conventions? I use lower case but what they use upper/lower
  and whitespaces.

* Need to check for resolution to add 1080p or 720p tags to filename.

* Need to add best guess if regex fails, like use the pms name instead.

* If 1080p or 720p then tag it.
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
VERSION_NUMBER = "0.5-1"
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
            #foreground colors
            'fg_light_grey': '\033[37m',
            'fg_dark_grey': '\033[90m',
            'fg_default': '\033[39m',
            # background colors
            'bg_light_grey': '\033[47m',
            'bg_dark_grey': '\033[100m',
            'bg_default': '\033[49m'
        }
        self.color_text = ''
        for style in user_styles:
            try:
                self.color_text += styles[style]
            except KeyError:
                pass

        self.color_text += text

    def __format__(self):
        return '\033[0m{}\033[0m'.format(self.color_text)

    @classmethod
    def light_grey(clazz, text):
        cls = clazz(text, fg_light_grey=True)
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
            # Skip colorizing filler lines with no data "-|-|-".
            if (((count % 2) == 0) and (colorize == True) and (not row_string.replace(" ", "").startswith("-|-|-"))):
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

def __analyze_tv_show(pms_tv_show, episode_container=""):
    logging.getLogger(MAIN_LOGGER_NAME).warning("The analyzing option is currently not working and the show: \"%s\" will not be analyzed." %(pms_tv_show.title))
    for season in pms.library.get(pms_tv_show.title).seasons():
        for episode in season.episodes():
            for ipart in episode.iter_parts():
                ipart_container = ipart.container
                if ((episode_container == ipart_container) or (not len(episode_container) > 0)):
                    ipart_filename = os.path.basename(ipart.file)
                    (path_to_tv_show, season_dir) = os.path.split(os.path.split(ipart.file)[0])
                    print "FILE: %s | %s | %s" %(os.path.basename(path_to_tv_show), season_dir, ipart_filename)
                    print "PMS:  %s(%s) | season_%s | %s" %(pms_tv_show.title.lower(), pms_tv_show.year, season.index, episode.index)
                    print "----------------"
                    # split season_d to (season, d) and case int(d) == pms.season.index
                    # Check tvshow directory contains year
                    # check container
                    # check title
                    # some might include epidsode title

def __print_tv_show(pms_tv_show, episode_container="", show_missing_details=False):
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
                            if ((episode_container == ipart_container) or (not len(episode_container) > 0)):
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
    cmdParser.add_option("-y", "--skip_confirmation",
                         action="store_true",
                         dest="disableConfirmation",
                         help="disables asking user for confirmation on certain actions",
                         default=False)
    cmdParser.add_option("-r", "--refresh",
                         action="store_true",
                         dest="refresh",
                         help="rescan or refresh all sections or specific section (-s)",
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
    cmdParser.add_option("-f", "--fix_filenames",
                         action="store_true",
                         dest="fix_filenames",
                         help="fix any filenames that are incorrect",
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
        examplesMessage = "\nExamples:"
        examplesMessage += "\nList all the sections.\n"
        examplesMessage += "$ %s -l\n" %(self.__commandName)
        examplesMessage += "\nList all the media of a certain type like show.\n"
        examplesMessage += "$ %s -t show\n" %(self.__commandName)
        examplesMessage += "\nList all the media for the section \"Movies\".\n"
        examplesMessage += "$ %s -s Movies\n" %(self.__commandName)
        examplesMessage += "\nList all the media that is a movie(type) using the container \"mkv\".\n"
        examplesMessage += "$ %s -t movie -c mkv\n" %(self.__commandName)
        examplesMessage += "\nAnalyze all the media in the section \"Documentaries\" that has a tv show called \"Some Show\". \nIn addition, the missing episodes for the tv show's season on PMS will be shown.\n"
        examplesMessage += "$ %s -s Documentaries -T \"Some Show\" -a -M\n" %(self.__commandName)
        self.print_version()
        OptionParser.print_help(self)
        print examplesMessage

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
        # Check for invalid options enabled.
        # #######################################################################
        if (len(cmdLineOpts.section_type) > 0) and (len(cmdLineOpts.section_name) > 0):
            logging.getLogger(MAIN_LOGGER_NAME).error("The -t and -s options cannot be used at the same time.")
            sys.exit(1)
        elif (not len(cmdLineOpts.section_type) > 0) and (not len(cmdLineOpts.section_name) > 0):
            logging.getLogger(MAIN_LOGGER_NAME).error("A value for the option -t or -s is required.")
            sys.exit(1)
        if ((cmdLineOpts.analyze) and (cmdLineOpts.show_missing_details)):
            logging.getLogger(MAIN_LOGGER_NAME).error("The -a and -M options cannot be used at the same time.")
            sys.exit(1)

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
            # convert to lowercase for comparing
            filename_extensions = [x.lower() for x in configParser.get("filenames", "filename_extensions").strip().split()]
        except ConfigParser.NoOptionError:
            # These can be skipped if not set.
            pass
        try:
            # convert to lowercase for comparing
            filename_tags = [x.lower() for x in configParser.get("filenames", "filename_tags").strip().split()]
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

        # Verify section_name exists if option has value.
        if (len(cmdLineOpts.section_name) > 0):
            found_section_name = False
            for section in pms.library.sections():
                if (section.title.lower().strip() == cmdLineOpts.section_name.lower().strip()):
                    found_section_name = True
            if (not found_section_name):
                logging.getLogger(MAIN_LOGGER_NAME).error("The section name does not exist: %s" %(cmdLineOpts.section_name))
                sys.exit(1)

        # Verify section_type exists if option has value.
        if (len(cmdLineOpts.section_type) > 0):
            found_section_type = False
            for section in pms.library.sections():
                if (section.type.lower().strip() == cmdLineOpts.section_type.lower().strip()):
                    found_section_type = True
            if (not found_section_type):
                logging.getLogger(MAIN_LOGGER_NAME).error("The section type does not exist: %s" %(cmdLineOpts.section_type))
                sys.exit(1)

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

        # #######################################################################
        # Refresh sections
        # #######################################################################
        if (cmdLineOpts.refresh):
            for section in pms.library.sections():
                if (((section.title == cmdLineOpts.section_name) or (section.type == cmdLineOpts.section_type)) or
                    (not (len(cmdLineOpts.section_name) > 0) and (not len(cmdLineOpts.section_type) > 0))):
                    logging.getLogger(MAIN_LOGGER_NAME).info("Refreshing section: %s(type: %s)" %(section.title, section.type))
                    section.refresh()

        # #######################################################################
        # Analyze or print metadata to console
        # #######################################################################
        # Just add analyze code to here, if analyze disabled then just print, if
        # analyze enable then analyze media and only output the tests that fail.
        logging.getLogger(MAIN_LOGGER_NAME).info("Fetching the metadata from sources can take some time, be patient.")
        if (cmdLineOpts.analyze):
            logging.getLogger(MAIN_LOGGER_NAME).info("Analzying is still work in progress.")
            for section in pms.library.sections():
                media_attributes = []
                if (section.type == "movie") and ((section.title == cmdLineOpts.section_name) or (section.type == cmdLineOpts.section_type)):
                    total_section_size = 0
                    for movie in section.all():
                        # Get file details about the file for this metadata.
                        #
                        #
                        # Need "requests.exceptions.Timeout" try/except catch over here for ipart query.
                        #
                        #
                        #from pprint import pprint
                        #def print_r(the_object):
                        #    print ("CLASS: ", the_object.__class__.__name__, " (BASE CLASS: ", the_object.__class__.__bases__,")")
                        #    pprint(vars(the_object))
                        #print_r(my_object)
                        #print "---"
                        for ipart in movie.iter_parts():
                            #
                            # NEED TO ADD THE RESOLUTION TAG to fileename
                            # NEED TO JUST COMAPRE FILENAMES on what code thinks should be and what it actually is
                            #
                            ipart_video_resolution = ipart.media.videoResolution
                            ipart_audio_codec = ipart.media.audioCodec       # Not sure i need to tag this either cause multiple audio streams
                            ipart_audio_channels = ipart.media.audioChannels # No need to tag those
                            print "%s (%s | %s | %s)" %( os.path.basename(ipart.file), ipart_video_resolution, ipart_audio_codec, ipart_audio_channels)
                            ipart_container = ipart.container
                            if ((cmdLineOpts.container == ipart_container) or (not len(cmdLineOpts.container) > 0)):
                                ipart_filename = os.path.basename(ipart.file)
                                # Need to finish breaking down to movie_title,
                                # year, tags, extenstion.
                                # regex = "^(?P<movie_title>[a-zA-Z_0-9\-+',&]*)\(.*(?P<year>[0-9]{4})\)(?P<tags>.*)\.(?P<extension>[a-zA-Z0-9]{3})"
                                # regex = "^(?P<movie_title>[a-zA-Z_0-9\-+',&]*)\((?P<year>[0-9]{4})\)(?P<tags>.*)\.(?P<extension>[a-zA-Z0-9]{3})"
                                # regex = "^(?P<movie_title>[a-zA-Z_0-9\-+', &\.]*)\((?P<year>[0-9]{4})\)(?P<tags>.*)\.(?P<extension>[a-zA-Z0-9]{3})"
                                # regex = "^(?P<movie_title>[a-zA-Z_0-9\-+', &\.]*)(\((?P<year>[0-9]{4})\)(?P<tags>.*))?\.(?P<extension>[a-zA-Z0-9]{3})"
                                # regex = "^(?P<movie_title>[a-zA-Z_0-9\-+', &\.]*)(\((?P<year>[0-9]{4})\))?(?P<tags>.*)?\.(?P<extension>[a-zA-Z0-9]{3})"
                                regex = "^(?P<movie_title>[a-zA-Z_0-9\-+', &\.]*)(\((?P<year>[0-9]{4})\))(?P<tags>.*)?\.(?P<extension>[a-zA-Z0-9]{3})"
                                rem = re.compile(regex)
                                mo = rem.match(ipart_filename)
                                if (not mo):
                                    logging.getLogger(MAIN_LOGGER_NAME).info("There was a parsing error for: \"%s\" but a best guess of PMS based filename will be attempted." %(ipart_filename))
                                    pms_modified_filename = "%s(%s)%s" %(__format_item(movie.title).lower().replace(" ", "_").replace(":_", ":"),
                                                                          __format_item(movie.year), os.path.splitext(ipart_filename)[1])
                                    media_attributes.append([ipart_filename, pms_modified_filename])
                                else:
                                    try:
                                        # If no year is given and just filename
                                        mo_year = "0000"
                                        try:
                                            mo_year = __format_item(mo.group("year")).lower()
                                        except IndexError:
                                            pass
                                        except AttributeError:
                                            pass
                                        pms_modified_filename = ""
                                        if ((not __format_item(movie.title).lower() == __format_item(mo.group("movie_title")).lower().replace("_", " ")) or
                                            (not str(__format_item(movie.year)).lower() == mo_year) or
                                            ((not __format_item(mo.group("extension")).lower() in filename_extensions) and (len(filename_extensions) > 0))):
                                            # Tags should follow this sequence:
                                            # ptX, resolution(1080p,720p), movie version(EE, DC)
                                            pms_tags = ""
                                            try:
                                                for tag in mo.group("tags").split("-"):
                                                    tag = tag.lower()
                                                    if (len(tag) > 0):
                                                        if ((tag in filename_tags) and (len(filename_tags) > 0)):
                                                            pms_tags = "%s-%s" %(pms_tags, tag)
                                                if (len(pms_tags) > 0):
                                                    pms_tags = pms_tags.rstrip("-")
                                            except IndexError:
                                                pass
                                            except AttributeError:
                                                pass
                                            pms_modified_filename = "%s(%s)%s.%s" %(__format_item(movie.title).lower().replace(" ", "_").replace(":_", ":"),
                                                                                    __format_item(movie.year), pms_tags,
                                                                                    mo.group("extension").lower())

                                        if (len(pms_modified_filename) > 0):
                                            media_attributes.append([ipart_filename, pms_modified_filename])
                                    except IndexError:
                                        media_attributes.append([ipart_filename, "?"])
                                        logging.getLogger(MAIN_LOGGER_NAME).debug("There was a parsing error for: \"%s\"" %(ipart_filename))
                    if (len(media_attributes) > 0):
                        media_attributes.insert(0, ["filename", "filename based on PMS"])
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

        else:
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
                            __print_tv_show(pms_tv_show, cmdLineOpts.container, cmdLineOpts.show_missing_details)
                        elif ((cmdLineOpts.tv_show_title.lower().strip() == pms_tv_show.title.strip().lower()) or
                              (cmdLineOpts.tv_show_title.lower().strip() == pms_tv_show.title.rsplit(" (")[0].strip().lower())):
                            # Allow for multiple tv shows with same name. For
                            # example BSG.org and BGS.2003. I assume that
                            # metadata is for the correct show. Running analyze
                            # should show if not.
                            __print_tv_show(pms_tv_show, cmdLineOpts.container, cmdLineOpts.show_missing_details)
    except KeyboardInterrupt:
        print ""
        message =  "This script will exit since control-c was executed by end user."
        logging.getLogger(MAIN_LOGGER_NAME).error(message)
        sys.exit(1)
    # #######################################################################
    # Exit the application with zero exit code since we cleanly exited.
    # #######################################################################
    sys.exit()
