#!/usr/bin/python
"""
# Need to find out all resolution variables that it can be then have option to
# only output for specific resolution or all and need to import optsparse and
# remove previous checks on argument and let oopts parse handle that.

# Add option to take path to csv file and add opts parse from skeleton default python file.
# REVIEW THIS. https://stackoverflow.com/questions/12296585/python-parse-csv-correctly

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
        rstring = "Season %s Episode %s | Container: %s | Resolution: %s" %(season, episode, container, self.__resolution)
        return rstring

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
        # Verfiy that a valid file was passed to it.
        # #######################################################################
        pathToCSVFile = ""
        try:
            pathToCSVFile = sys.argv[1]
        except IndexError:
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
                            tvEpisode = TVEpisode(int(row.get("Season")), int(row.get("Episode")),  row.get("Height"),
                                                  row.get("Width"),  row.get("Video Resolution"),  row.get("Video FrameRate"),
                                                  row.get("Aspect Ratio"),  row.get("Bit Rate"),  row.get("Video Codec"),
                                                  row.get("Container"))
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
        for tvShow in tvShows:
            print mapOfTVShows.get(tvShow)

    except KeyboardInterrupt:
        print ""
        message =  "This script will exit since control-c was executed by end user."
        logging.getLogger(MAIN_LOGGER_NAME).error(message)
        sys.exit(1)
    # #######################################################################
    # Exit the application with zero exit code since we cleanly exited.
    # #######################################################################
    sys.exit()
