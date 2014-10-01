#!/usr/bin/python
"""
# This script will output a list of TV Show Episodes that are in a specific resolution.

# * Need to find out all resolution variables that it can be then have option to
# * Give examples on how to use multiple resolutions.
# * Add container code
# * Add check that cannot do container and resolution at same time or add code to allow this.

# https://forums.plex.tv/index.php/topic/103115-rel-plex2csv/
# https://forums.plex.tv/index.php/topic/115637-how-to-print-listing-of-all-tv-shows-episodes-resolution/

@author    : Shane Bradley
@contact   : sbradley@redhat.com
@version   : 0.1
@copyright : GPLv2

"""
import sys
import logging
import logging.handlers
import os
import os.path
from optparse import OptionParser, Option, SUPPRESS_HELP


import csv
from collections import defaultdict

# #####################################################################
# Global vars:
# #####################################################################
VERSION_NUMBER = "0.1-1"
MAIN_LOGGER_NAME = "%s" %(os.path.basename(sys.argv[0]))

METADATA_FIELDS = ["Series Title", "Season", "Episode", "Height", "Width",
                   "Video Resolution", "Video FrameRate", "Aspect Ratio",
                   "Bit Rate", "Video Codec", "Container"]

# ##############################################################################
# Containers for Metadata
# ##############################################################################
class TVShow:
    def __init__(self, name):
        self.__name = name
        self.__listOfEpisodes = []

    def __str__(self):
        rstring = "%s: %d episodes" %(self.getName(), self.count())
        for tvEpisode in self.list():
            rstring += "\n  %s" %(tvEpisode)
        return rstring

    def getName(self):
        return self.__name

    def list(self):
        return self.__listOfEpisodes

    # Need compare for sorting
    def sort(self):
        pass

    def count(self):
        return len(self.list())

    def add(self, tvEpisode):
        # Need to check if it already exists.
        self.__listOfEpisodes.append(tvEpisode)

class TVEpisode:
    def __init__(self, season, episode, height, width, resolution, framerate, aspectRatio, bitRate, codec, container):
        # int
        self.__season = season
        # int
        self.__episode = episode
        self.__height = height
        self.__width = width
        self.__resolution = resolution
        self.__framerate = framerate
        self.__aspectRatio = aspectRatio
        self.__bitRate = bitRate
        self.__codec = codec
        self.__container = container

    def __str__(self):
        season = ""
        # The season will be  4 characters.
        if (self.__season < 100):
            season = "  %02d " %(self.__season)
        else:
            season = "%04d " %(self.__season)
        episode = ""
        # The episode will be 3 characters.
        if (self.__episode < 100):
            episode ="%02d " %(self.__episode)
        else:
            episode = "%03d " %(self.__episode)

        # The container will be 3 characters.
        container = self.__container
        if (not len(container) > 0):
            container = "   "
        # Return the formatted string
        rstring = "Season %s Episode %s | Container: %s | Resolution: %s(%sx%s)" %(season, episode, container, self.__resolution, self.__height, self.__width)
        return rstring

    def getSeason(self):
        return self.__season

    def getEpisode(self):
        return self.__episode

    def getHeight(self):
        return self.__height

    def getWidth(self):
        return self.__width

    def getResolution(self):
        return self.__resolution

    def getFrameRate(self):
        return self.__framerate

    def getAspectRatio(self):
        return self.__aspectRatio

    def getBitRate(self):
        return self.__bitRate

    def getCodec(self):
        return self.__codec

    def getContainer(self):
        return self.__container

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
    cmdParser.add_option("-y", "--no_ask",
                        action="store_true",
                         dest="disableQuestions",
                         help="disables all questions and assumes yes",
                         default=False)
    cmdParser.add_option("-f", "--path_to_filename",
                         action="store",
                         dest="pathToCSVFile",
                         help="the path to the .csv file that will be parsed",
                         type="string",
                         metavar="<input filename>",
                         default="")
    cmdParser.add_option("-r", "--resolution",
                         action="extend",
                         dest="listOfResolutions",
                         help="query for metadata that has the following resolution for the video files(The valid values are: sd, 480, 720, 1080, if empty all resolutions are printed)",
                         type="string",
                         metavar="<resolution>",
                         default=[])

    #cmdParser.add_option("-c", "--container",
    #                     action="extend",
    #                     dest="listOfContainers",
    #                     help="query for metadata that has the following container for the video files",
    #                     type="string",
    #                     metavar="<container>",
    #                     default=[])

 # Get the options and return the result.
    (cmdLineOpts, cmdLineArgs) = cmdParser.parse_args()
    return (cmdLineOpts, cmdLineArgs)

# ##############################################################################
# OptParse classes for commandline options
# ##############################################################################
class OptionParserExtended(OptionParser):
    def __init__(self, version) :
        self.__commandName = os.path.basename(sys.argv[0])
        versionMessage = "%s %s\n" %(self.__commandName, version)

        commandDescription  ="%s will list metadata information related to video file properties for a .csv file created by Plex2cvs with extreme setting for TV Shows.\n"%(self.__commandName)

        OptionParser.__init__(self, option_class=ExtendOption,
                              version=versionMessage,
                              description=commandDescription)

    def print_help(self):
        self.print_version()
        examplesMessage = "\n"
        OptionParser.print_help(self)
        #print examplesMessage

class ExtendOption (Option):
    ACTIONS = Option.ACTIONS + ("extend",)
    STORE_ACTIONS = Option.STORE_ACTIONS + ("extend",)
    TYPED_ACTIONS = Option.TYPED_ACTIONS + ("extend",)

    def take_action(self, action, dest, opt, value, values, parser):
        if (action == "extend") :
            valueList = []
            try:
                for v in value.split(","):
                    newValue = v.strip().rstrip()
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
        # Set the logging levels based on user options.
        # #######################################################################
        if ((cmdLineOpts.enableDebugLogging) and (not cmdLineOpts.disableLoggingToConsole)):
            logging.getLogger(MAIN_LOGGER_NAME).setLevel(logging.DEBUG)
            streamHandler.setLevel(logging.DEBUG)
            message = "Debugging has been enabled."
            logging.getLogger(MAIN_LOGGER_NAME).debug(message)
        if (cmdLineOpts.disableLoggingToConsole):
            streamHandler.setLevel(logging.CRITICAL)

        # #######################################################################
        # Verfiy that a valid file was passed to it.
        # #######################################################################
        pathToCSVFile = cmdLineOpts.pathToCSVFile
        if (not len(pathToCSVFile) > 0):
            message = "There was no path given as an argument."
            logging.getLogger(MAIN_LOGGER_NAME).error(message)
            message = "A path to a exported .csv file of Plex metadata is required."
            logging.getLogger(MAIN_LOGGER_NAME).info(message)
            sys.exit(1)

        if (not os.path.isfile(pathToCSVFile)):
            message = "The argument %s is not a file." %(pathToCSVFile)
            logging.getLogger(MAIN_LOGGER_NAME).error(message)
            message = "A path to a exported .csv file of Plex metadata is required."
            logging.getLogger(MAIN_LOGGER_NAME).info(message)
            sys.exit(1)

        # Verify the .cvs has a header with the required fields.
        try:
            with open(pathToCSVFile, "r") as f:
                metadataFieldsInFile = f.readline().strip().replace("\"", "").split(",")
                # Verify the metadata fields are in the file.
                missing_metadata_fields = ""
                for mf in METADATA_FIELDS:
                    if (not mf in metadataFieldsInFile):
                        missing_metadata_fields += "\"%s\"" %(mf)
                if (len(missing_metadata_fields) > 0):
                    message = "The following metadata fields were not in the file %s: %s." %(pathToCSVFile, missing_metadata_fields)
                    logging.getLogger(MAIN_LOGGER_NAME).error(message)
                    message = "A path to a exported .csv file of Plex metadata is required."
                    logging.getLogger(MAIN_LOGGER_NAME).info(message)
                    sys.exit(1)

        except IOError:
            message = "There was an error reading the file: %s" %(pathToCSVFile)
            logging.getLogger(MAIN_LOGGER_NAME).error(message)
            sys.exit(1)
        finally:
            f.close()

        # #######################################################################
        #  Read the file and create a map of objects.
        # #######################################################################

        message = "Analyzing the .csv file: %s" %(pathToCSVFile)
        logging.getLogger(MAIN_LOGGER_NAME).info(message)

        mapOfTVShows = {}
        try:
            with open(pathToCSVFile) as f:
                reader = csv.DictReader(f) # read rows into a dictionary format
                for row in reader: # read a row as {column1: value1, column2: value2,...}
                    seriesTitle = row.get("Series Title").replace("\n", " ")
                    # Add add tv shows that have a series title, and skip if they do not.
                    if (len(seriesTitle) > 0):
                        try:
                            tvEpisode = TVEpisode(int(row.get("Season")), int(row.get("Episode")), row.get("Height").strip(),
                                                  row.get("Width").strip(), row.get("Video Resolution").strip(), row.get("Video FrameRate").strip(),
                                                  row.get("Aspect Ratio").strip(), row.get("Bit Rate").strip(), row.get("Video Codec").strip(),
                                                  row.get("Container").strip())
                            if (not mapOfTVShows.has_key(seriesTitle)):
                                mapOfTVShows[seriesTitle] = TVShow(seriesTitle)
                            mapOfTVShows[seriesTitle].add(tvEpisode)
                        except ValueError:
                            # If casting a string value to integer fails then skip the episode.
                            pass
        except IOError:
            message = "There was an error reading the file: %s" %(pathToCSVFile)
            logging.getLogger(MAIN_LOGGER_NAME).error(message)
            sys.exit(1)
        finally:
            f.close()
        # Print TV Shows
        tvShows = mapOfTVShows.keys()
        tvShows.sort()
        for key in tvShows:
            tvShow = mapOfTVShows.get(key)
            if (not len(cmdLineOpts.listOfResolutions) > 0):
                print tvShow
            else:
                sEpisodes = ""
                for episode in tvShow.list():
                    if (episode.getResolution() in cmdLineOpts.listOfResolutions):
                        sEpisodes += "\n  %s" %(episode)
                if (len(sEpisodes) > 0):
                    print "%s: %d episodes%s\n" %(tvShow.getName(), tvShow.count(), sEpisodes)

    except KeyboardInterrupt:
        print ""
        message =  "This script will exit since control-c was executed by end user."
        logging.getLogger(MAIN_LOGGER_NAME).error(message)
        sys.exit(1)
    # #######################################################################
    # Exit the application with zero exit code since we cleanly exited.
    # #######################################################################
    sys.exit()
