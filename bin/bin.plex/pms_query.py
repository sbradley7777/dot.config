#!/usr/bin/python
"""
This script will perform various queries to the Plex Media Server. The script
uses the API supplied by this project which will need to be installed:
- https://github.com/mjs7231/python-plexapi

# sudo pip install plexapi

Edit the configuration file to add in username and password.
$ cat ~/.pms_connect.conf
[login]
username = <login>
password = <password>
pms_name = <name of plex media server>

TODO:
* Add code to find missing tv shows for a show. For longer shows, put an option
  to show last 20 seasons or something. Show missing tv shows episodes for only
  the seasons I have or option to show all missing episodes.
* Add total GB size for all episodes in season.
* Add better formatter to format tables so that files shows up better. Maybe
  should format everything to GB formatted float.

@author    : Shane Bradley
@contact   : sbradley@redhat.com
@version   : 0.1
@copyright : GPLv2
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

try:
    from plexapi.myplex import MyPlexUser
    from plexapi.exceptions import NotFound
except ImportError:
    print "Error: There was an error import the library \"plexapi\". The library is required to be installed."
    print "Info:  The library needs to be installed and is located here: https://github.com/mjs7231/python-plexapi"
    sys.exit(1)

# #####################################################################
# Global vars:
# #####################################################################
VERSION_NUMBER = "0.1-1"
MAIN_LOGGER_NAME = "%s" %(os.path.basename(sys.argv[0]))
CONFIG_FILE = os.path.expanduser("~/.pms_query.conf")

# #####################################################################
# Helper classes
# #####################################################################
class TableFormatter:
    # #######################################################################
    # Functions for creating a formatted table from lists of lists:
    # #######################################################################
    def __formatTableValue(self, tableValue):
        locale.setlocale(locale.LC_NUMERIC, "")

        try:
            if type(tableValue) is int:
                inum = int(tableValue)
                return locale.format("%.*f", (0, inum), True)
            else:
                return str(tableValue)
        except UnicodeEncodeError:
            return tableValue.encode("utf-8")

    def __getMaxColumnWidth(self, table, index):
        try:
            return max([len(self.__formatTableValue(row[index])) for row in table])
        except IndexError:
            return -1

    def toTableString(self, table, headerList=None):
        if (not len(table) > 0):
            return ""
        tString = ""
        tableStringsList = self.formatStringListsToTable(table, headerList)
        for tableStrings in tableStringsList:
            currentLine = ""
            for ts in tableStrings:
                currentLine += ts
            tString += "%s\n" %(currentLine)
        return tString.rstrip()

    def formatStringListsToTable(self, table, headerList=None):
        """
        This function will take an array of arrays and then output a
        single string that is a formatted table.

        An empty list will be returned if the column count is not the
        same for each row in the table.

        I got code from this url and modified it:
        http://ginstrom.com/scribbles/2.17/09/04/pretty-printing-a-table-in-python/

        Example(added spacing to make example clear):
        table = [["",       "names", "birthyear", "age"], ["NCuser", "bob",   1976,         35]]
        """
        # Copy the table so that we do not ref the orginal list and change it.
        copyOfTable = []
        for currentList in table:
            newList = []
            for item in currentList:
                newList.append(item)
            if (len(newList) > 0):
                copyOfTable.append(newList)

        # Return empty list and print error if all the rows in table
        # dont have same column count.
        if (len(copyOfTable) > 0):
            # Add header to the list if one was passed to it and table is not empty.
            if (not headerList == None):
                copyOfTable.insert(0, headerList)
            # Make sure that table and header contain the same number
            # of columns.
            colCount = len(copyOfTable[0])
            for currentRow in copyOfTable:
                currentColCount = len(currentRow)
                if (not (currentColCount == colCount)):
                    message = "The table contains columns with a different columns counts and will not be processed."
                    logging.getLogger(sx.MAIN_LOGGER_NAME).debug(message)
                    return []
        else:
            message = "The table contains no rows in the table and will not be processed."
            logging.getLogger(sx.MAIN_LOGGER_NAME).debug(message)
            return []
        # This function will append new rows if the item in the column
        # for a row is an array/list. If there is a None value or
        # empty string then replace with "-" for no value rep.
        currentRowIndex = 0
        for currentRow in copyOfTable:
            newRows = []
            currentColIndex = 0
            for currentCol in currentRow:
                if (currentCol == None):
                   currentRow[currentColIndex] = "-"
                elif (not (len(currentCol) > 0)):
                    currentRow[currentColIndex] = "-"
                elif (type(currentCol) == list):
                    currentColList = currentCol
                    if (len(currentCol) > 0):
                        currentRow[currentColIndex] = currentColList.pop(0)
                    for ccListIndex in range(0, len(currentColList)):
                        try:
                            newRows[ccListIndex][currentColIndex] = currentColList[ccListIndex]
                        except IndexError:
                            newRow = []
                            for i in range(len(currentRow)):
                                newRow[i] = ""
                            newRow[currentColIndex] = currentColList[ccListIndex]
                            newRows.append(newRow)
                currentColIndex = currentColIndex + 1
            for row in newRows:
                copyOfTable.insert(currentRowIndex + 1,row)
            currentRowIndex = currentRowIndex  + 1
        # Fix the max spacing for each column after iterating over
        # each row.
        tableStringsList = []
        col_paddings = []
        for i in range(len(copyOfTable[0])):
            maxColumnWidth = self.__getMaxColumnWidth(copyOfTable, i)
            if (maxColumnWidth >= 0):
                col_paddings.append(maxColumnWidth)
        # If header was given then use the max column size to build
        # the seperator.
        if (not headerList == None):
            headerSeperatorList = []
            for colMaxSize in col_paddings:
                currentHeaderSeperator = ""
                currentHeaderSeperator += "-" * colMaxSize
                headerSeperatorList.append(currentHeaderSeperator)
            copyOfTable.insert(1, headerSeperatorList)
        for row in copyOfTable:
            # Left col string has no spacing.
            tableStrings = []
            try:
                tableStrings.append( str(row[0].ljust(col_paddings[0] + 1)))
            except UnicodeEncodeError:
                print row
            # Add spacing to the rest of the columns.
            for i in range(1, len(row)):
                # Add spacing to to the right side with ljust.
                try:
                    tableStrings.append(str(self.__formatTableValue(row[i]).ljust(col_paddings[i] + 2)))
                except IndexError:
                    continue
            tableStringsList.append(tableStrings)
        return tableStringsList

# ##############################################################################
# Helper functions
# ##############################################################################
def humanize_file_size(size):
    size = abs(size)
    if (size==0):
        return "0B"
    units = ['B','KiB','MiB','GiB','TiB','PiB','EiB','ZiB','YiB']
    p = math.floor(math.log(size, 2)/10)
    return "%.3f%s" % (size/math.pow(1024,p),units[int(p)])

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
    cmdParser.add_option("-s", "--section_name",
                         action="store",
                         dest="section_name",
                         help="name of the section",
                         type="string",
                         metavar="<section name>",
                         default="")
    cmdParser.add_option("-m", "--movies",
                         action="store_true",
                         dest="movies_query",
                         help="query movies",
                         default=False)
    cmdParser.add_option("-t", "--tv_shows",
                         action="store_true",
                         dest="tv_shows_query",
                         help="query tv shows",
                         default=False)
    cmdParser.add_option("-T", "--tv_show_title",
                         action="store",
                         dest="tv_show_title",
                         help="title of the tv show",
                         type="string",
                         metavar="<title of tv show>",
                         default="")
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
        username = configParser.get("login", "username").strip()
        password = configParser.get("login", "password").strip()
        pms_name = configParser.get("login", "pms_name").strip()

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
        plex_user = MyPlexUser.signin(username, password)
        pms = plex_user.getResource(pms_name).connect()

        found_section_name = False
        if ( len(cmdLineOpts.section_name) > 0):
            for section in pms.library.sections():
                if (section.title == cmdLineOpts.section_name):
                    found_section_name = True
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
            table_formatter = TableFormatter()
            print table_formatter.toTableString(sections, ["-", "Section Name", "Section Type"])

        # #######################################################################
        # Query Movies
        # #######################################################################
        if (cmdLineOpts.movies_query):
            logging.getLogger(MAIN_LOGGER_NAME).info("Searching for movies on the pms %s" %(pms_name))
            for section in pms.library.sections():
                movies_attributes = []
                if (section.type == "movie") and ((section.title == cmdLineOpts.section_name) or
                                                  (not len(cmdLineOpts.section_name) > 0)):
                    counter = 1
                    for video in section.all():
                        ccounter = counter
                        # Get file details about the file for this metadata.
                        for ipart in video.iter_parts():
                            ipart_container = ipart.container
                            if ((cmdLineOpts.container == ipart_container) or (not len(cmdLineOpts.container) > 0)):
                                ipart_filename = os.path.basename(ipart.file)
                                ipart_size = humanize_file_size(ipart.size)
                                movies_attributes.append([str(ccounter), video.title, str(video.year), ipart_container, ipart_filename, ipart_size])
                                # Dont increase count but replace with dash.
                                ccounter = "-"
                        if (ccounter == "-"):
                            counter = counter + 1;
                if (len(movies_attributes) > 0):
                    table_formatter = TableFormatter()
                    print table_formatter.toTableString(movies_attributes, ["%s" %(section.title), "Movie Name", "Year", "Container", "Filename", "Size"])
                    print
                else:
                    logging.getLogger(MAIN_LOGGER_NAME).error("There was no movies metadata found.")

        # #######################################################################
        # Query TV Shows
        # #######################################################################
        if (cmdLineOpts.tv_shows_query):
            logging.getLogger(MAIN_LOGGER_NAME).info("Searching for tv shows on the pms %s" %(pms_name))
            output = ""
            for section in pms.library.sections():
                tv_show_attributes = []
                if (section.type == "show") and ((section.title == cmdLineOpts.section_name) or
                                                  (not len(cmdLineOpts.section_name) > 0)):
                    counter = 1
                    for video in section.all():
                        title = video.title.split(" (")[0]
                        if (not len(cmdLineOpts.tv_show_title) > 0):
                            try:
                                output += "%02d: %s(%d) [Seasons: %02d] [Episodes: %02d]\n" %(counter, title, video.year, len(pms.library.get(video.title).seasons()), len(pms.library.get(video.title).episodes()))
                                try:
                                    for season in pms.library.get(video.title).seasons():
                                        output += "\t%s (Episodes: %d)\n" %(season.title, len(season.episodes()))
                                except requests.exceptions.ConnectionError as e:
                                    logging.getLogger(MAIN_LOGGER_NAME).debug("The metadata for the seasons failed: %s" %(video.title))
                            except NotFound:
                                output += "%02d: %s(%d) (Warning: data not found for TV Show).\n" %(counter, title, video.year)
                            counter = counter + 1
                        elif (cmdLineOpts.tv_show_title.lower().strip() == title.lower().strip()):
                            # Allow for multiple tv shows with same name. For example BSG.org and BGS.2003
                            output += "%s(%d) [Seasons: %02d] [Episodes: %02d]\n" %(title, video.year, len(pms.library.get(video.title).seasons()), len(pms.library.get(video.title).episodes()))
                            try:
                                for season in pms.library.get(video.title).seasons():
                                    output += "\t%s (Episodes: %d)\n" %(season.title, len(season.episodes()))
                            except requests.exceptions.ConnectionError as e:
                                logging.getLogger(MAIN_LOGGER_NAME).debug("The metadata for the seasons failed: %s" %(video.title))
            if (len(output) > 0):
                print output
            else:
                if (len(cmdLineOpts.tv_show_title) > 0):
                    logging.getLogger(MAIN_LOGGER_NAME).error("There was no metadata found matching the tv show: %s" %(cmdLineOpts.tv_show_title))
                else:
                    logging.getLogger(MAIN_LOGGER_NAME).error("There was no tv show metadata found.")


    except KeyboardInterrupt:
        print ""
        message =  "This script will exit since control-c was executed by end user."
        logging.getLogger(MAIN_LOGGER_NAME).error(message)
        sys.exit(1)
    # #######################################################################
    # Exit the application with zero exit code since we cleanly exited.
    # #######################################################################
    sys.exit()
