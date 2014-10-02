#!/usr/bin/python
"""
# This script will output a list of TV Show Episodes that are in a specific resolution or container.

# TODO:
# * Add everything to do movie parsing except the actual parsing and output.
# * Update help to say does movie support once updated.

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
VERSION_NUMBER = "0.2-1"
MAIN_LOGGER_NAME = "%s" %(os.path.basename(sys.argv[0]))

METADATA_FIELDS = ["Height", "Width", "Video Resolution", "Video FrameRate",
                   "Aspect Ratio", "Bit Rate", "Video Codec", "Container", "Year"]

METADATA_FIELDS_TV_SHOW = ["Series Title", "Season", "Episode"]
METADATA_FIELDS_MOVIE = ["Title"]

# ##############################################################################
# Containers for Metadata
# ##############################################################################
class Metadata:
    def __init__(self, height, width, resolution, framerate, aspectRatio, bitRate, codec, container):
        self.__height = height
        self.__width = width
        self.__resolution = resolution
        self.__framerate = framerate
        self.__aspectRatio = aspectRatio
        self.__bitRate = bitRate
        self.__codec = codec
        self.__container = container

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

class Movie(Metadata):
    def __init__(self, title, height, width, resolution, framerate, aspectRatio, bitRate, codec, container):
        self.__title = title
        Metadata.__init__(self,  height, width, resolution, framerate, aspectRatio, bitRate, codec, container)

    def __str__(self):
        return self.getTitle()

    def getTitle(self):
        return self.__title

class TVEpisode(Metadata):
    def __init__(self, season, episode, height, width, resolution, framerate, aspectRatio, bitRate, codec, container):
        # int
        self.__season = season
        # int
        self.__episode = episode
        Metadata.__init__(self,  height, width, resolution, framerate, aspectRatio, bitRate, codec, container)

    def __str__(self):
        season = ""
        # The season will be  4 characters.
        if (self.getSeason() < 100):
            season = "  %02d " %(self.getSeason())
        else:
            season = "%04d " %(self.getSeason())
        episode = ""
        # The episode will be 3 characters.
        if (self.getEpisode() < 100):
            episode ="%02d " %(self.getEpisode())
        else:
            episode = "%03d " %(self.getEpisode())

        # The container will be 3 characters.
        container = self.getContainer()
        if (not len(container) > 0):
            container = "   "
        # Return the formatted string
        rstring = "Season %s Episode %s | Container: %s | Resolution: %s (%shx%sw)" %(season, episode, container, self.getWidth(), self.getHeight(), self.getWidth())
        return rstring

    def getSeason(self):
        return self.__season

    def getEpisode(self):
        return self.__episode

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

# ##############################################################################
# Private Functions
# ##############################################################################
def parseTVShows(pathToCSVFile):
    mapOfTVShows = {}
    try:
        with open(pathToCSVFile) as f:
            reader = csv.DictReader(f)
            for row in reader:
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
    finally:
        f.close()
    return mapOfTVShows

def parseMovies(pathToCSVFile):
    listOfMovies = []
    try:
        with open(pathToCSVFile) as f:
            reader = csv.DictReader(f)
            for row in reader:
                # Do parsing here into movie object.
                pass
    except IOError:
        message = "There was an error reading the file: %s" %(pathToCSVFile)
        logging.getLogger(MAIN_LOGGER_NAME).error(message)
    finally:
        f.close()
    return listOfMovies

def printTVShows(mapOfTVShows, listOfResolutions, listOfContainers):
    # Print TV Shows
    tvShows = mapOfTVShows.keys()
    tvShows.sort()
    for key in tvShows:
        tvShow = mapOfTVShows.get(key)
        if ((not len(listOfResolutions) > 0) and (not len(listOfContainers) > 0)):
            print tvShow
        else:
            sEpisodes = ""
            for episode in tvShow.list():
                if (len(listOfResolutions) > 0):
                    if (episode.getResolution() in listOfResolutions):
                        sEpisodes += "\n  %s" %(episode)
                elif (len(listOfContainers) > 0):
                    if (episode.getContainer() in listOfContainers):
                        sEpisodes += "\n  %s" %(episode)
            if (len(sEpisodes) > 0):
                print "%s: %d episodes%s\n" %(tvShow.getName(), tvShow.count(), sEpisodes)

def printMovies(listOfMovies, listOfResolutions, listOfContainers):
    print "Parsing movie metadata is not supported yet."

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
    cmdParser.add_option("-c", "--container",
                         action="extend",
                         dest="listOfContainers",
                         help="query for metadata that has the following container for the video files",
                         type="string",
                         metavar="<container>",
                         default=[])

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

        commandDescription  ="%s will list metadata information related to video file properties for a .csv file created by Plex2cvs with extreme(or higher) setting for TV Shows.\n"%(self.__commandName)

        OptionParser.__init__(self, option_class=ExtendOption,
                              version=versionMessage,
                              description=commandDescription)

    def print_help(self):
        self.print_version()
        OptionParser.print_help(self)
        examplesMessage = "\n"
        examplesMessage += "This example will list the metadata that has attribute for the resolution set to \"sd\" or \"480\".\n"
        examplesMessage += "$ %s -f ~/Documentaries.csv  -r sd,480\n" %(self.__commandName)
        examplesMessage += "This example will list the metadata that has attribute for the container set to \"avi\" or \"mkv\".\n"
        examplesMessage += "$ %s -f ~/Documentaries.csv  -r avi,mkv\n" %(self.__commandName)
        print examplesMessage

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
        # The option -c and -r cannot be used at the same time.
        if ((len(cmdLineOpts.listOfContainers) > 0) and (len(cmdLineOpts.listOfResolutions) > 0)):
            message = "The option \"-c\" and \"-r\" cannot be used at the same time."
            logging.getLogger(MAIN_LOGGER_NAME).error(message)
            sys.exit(1)
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

        # Verify the .cvs has a header with the required fields and set the type of metadata.
        metadataType = ""
        try:
            with open(pathToCSVFile, "r") as f:
                metadataFieldsInFile = f.readline().strip().replace("\"", "").split(",")
                # Verify the metadata fields are in the files and find out if movie or tvshow.
                missing_metadata_fields = ""
                if (not set(METADATA_FIELDS_TV_SHOW).isdisjoint(metadataFieldsInFile)):
                    metadataType = "tvshow"
                    for mf in (METADATA_FIELDS + METADATA_FIELDS_TV_SHOW):
                        if (not mf in metadataFieldsInFile):
                            missing_metadata_fields += "\"%s\"" %(mf)
                elif (not set(METADATA_FIELDS_MOVIE).isdisjoint(metadataFieldsInFile)):
                    metadataType = "movie"
                    for mf in (METADATA_FIELDS + METADATA_FIELDS_MOVIE):
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

        if (metadataType == "tvshow"):
            mapOfTVShows = parseTVShows(pathToCSVFile)
            if (not len(mapOfTVShows) > 0):
                message = "There was no results to print."
                logging.getLogger(MAIN_LOGGER_NAME).info(message)
            else:
                printTVShows(mapOfTVShows, cmdLineOpts.listOfResolutions, cmdLineOpts.listOfContainers)
        elif (metadataType == "movie"):
            listOfMovies = parseMovies(pathToCSVFile)
            if (not len(listOfMovies) > 0):
                message = "There was no results to print."
                logging.getLogger(MAIN_LOGGER_NAME).info(message)
            else:
                printMovies(listOfMovies, cmdLineOpts.listOfResolutions, cmdLineOpts.listOfContainers)
        else:
            message = "Unknown metadata type. Currently only supporting TV Shows metadata in a csv file."
            logging.getLogger(MAIN_LOGGER_NAME).error(message)
            sys.exit(1)
    except KeyboardInterrupt:
        print ""
        message =  "This script will exit since control-c was executed by end user."
        logging.getLogger(MAIN_LOGGER_NAME).error(message)
        sys.exit(1)
    # #######################################################################
    # Exit the application with zero exit code since we cleanly exited.
    # #######################################################################
    sys.exit()
