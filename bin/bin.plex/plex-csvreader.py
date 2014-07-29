#!/usr/bin/env python
"""
TODO:
* Need to have the resolution sorted, so create a dict for each resolution typle then do a sort.
* Need to find out what out which are HD and maybe add option for that.
* Add getopts to this script.

URLS:
* https://forums.plex.tv/index.php/topic/115637-how-to-print-listing-of-all-tv-shows-episodes-resolution/
* https://stackoverflow.com/questions/12296585/python-parse-csv-correctly

@author   :  Shane Bradley
@contact  :
@version  :  1.00
"""
import sys
import os
import os.path
import string
import logging
import logging.handlers
import csv

# #####################################################################
# Global vars:
# #####################################################################
VERSION_NUMBER = "1.0"
MAIN_LOGGER_NAME = "%s" %(os.path.basename(sys.argv[0]))

class StringUtil:
    # #######################################################################
    # Functions for creating a formatted table from lists of lists:
    # #######################################################################
    def __getMaxColumnWidth(self, table, index):
        try:
            return max([len(str(row[index])) for row in table])
        except IndexError:
            return -1

    def formatStringListsToTable(self, table, headerList=None):
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
                    message = "The table continues columns with a different columns counts and will not be processed."
                    logging.getLogger(sx.MAIN_LOGGER_NAME).debug(message)
                    return []
        else:
            message = "The table continues no rows in the table and will not be processed."
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
            tableStrings.append( str(row[0].ljust(col_paddings[0] + 1)))
            # Add spacing to the rest of the columns.
            for i in range(1, len(row)):
                # Add spacing to to the right side with ljust.
                try:
                    tableStrings.append(str(str(row[i]).ljust(col_paddings[i] + 2)))
                except IndexError:
                    continue
            tableStringsList.append(tableStrings)
        return tableStringsList

    def toTableStringsList(self, table, headerList=None):
        tableOfStrings = []
        tableStringsList = self.formatStringListsToTable(table, headerList)
        for tableStrings in tableStringsList:
            currentLine = ""
            for ts in tableStrings:
                currentLine += ts
            tableOfStrings.append(currentLine)
        return tableOfStrings

# #######################################################################
# Helper Functions
# #######################################################################
def __dequote(s):
    """
    If a string has single or double quotes around it, remove them.
    If a matching pair of quotes is not found, return the string unchanged.
    """
    if (s.startswith(("'", '"')) and s.endswith(("'", '"')) and (s[0] == s[-1])):
        s = s[1:-1]
    return s

def __colorizeBackground(text):
    # Dark Gray
    # http://misc.flogisoft.com/bash/tip_colors_and_formatting
    bgColor = "100"
    opencol = "\033["
    closecol = "m"
    clear = opencol + "0" + closecol
    bg = opencol + bgColor + closecol
    return "%s%s%s" % (bg, text, clear)

def __usage():
    print "Usage:\n\t%s <path to csv file that contains export data from Plex>" %(os.path.basename(sys.argv[0]))

# ###############################################################################
# Main Function
# ###############################################################################
if __name__ == "__main__":
    try:
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

        path_to_filename = ""
        try: 
            path_to_filename = str(sys.argv[1])
        except IndexError:
            message =  "There was no path to a csv file passed to the command as an argument."
            logging.getLogger(MAIN_LOGGER_NAME).error(message)
            __usage()
            sys.exit(1)
        print path_to_filename

        message = "The parsing of the csv will start."
        logging.getLogger(MAIN_LOGGER_NAME).info(message)

        if (not os.path.exists(path_to_filename)):
            message = "The path to the csv file does not exists: %s." %(path_to_filename)
            logging.getLogger(MAIN_LOGGER_NAME).error(message)
            __usage()
            sys.exit(1)

        data = []
        try:
            csv_file = open(path_to_filename, "r")
            data = csv_file.readlines()
        except IOError:
            message = "There was an error reading the file: %s." %(path_to_filename)
            logging.getLogger(MAIN_LOGGER_NAME).error(message)
            __usage()
            sys.exit(1)

        csv_fields = []
        if (not len(data) > 0):
            message = "There was no data to parse for file: %s." %(path_to_filename)
            logging.getLogger(MAIN_LOGGER_NAME).error(message)
            __usage()
            sys.exit(1)
        else:
            for s in data.pop(0).split(","):
                csv_fields.append(__dequote(s))

        if (not len(csv_fields) > 0):
            message = "The field attribute names were not found."
            logging.getLogger(MAIN_LOGGER_NAME).warning(message)

        message = "There was %d records in the csv file." %(len(data))
        logging.getLogger(MAIN_LOGGER_NAME).info(message)


        table = []
        tableHD = []
        dict_reader = csv.DictReader(data, fieldnames = csv_fields, delimiter = ',', quotechar = '"')
        for row in dict_reader:
            series_title = row.get("Series Title").strip().rstrip().replace("\n", "")
            season = row.get("Season").strip().rstrip().replace("\n", "")
            episode = row.get("Episode").strip().rstrip().replace("\n", "")
            video_resolution = row.get("Video Resolution").strip().rstrip().replace("\n", "")
            if (len(series_title) > 0):
                if ((video_resolution == "720") or (video_resolution == "1080")):
                    tableHD.append([series_title, season, episode, video_resolution])
                else:
                    table.append([series_title, season, episode, video_resolution])

        stringUtil = StringUtil()
        index = 1
        if (len(table) > 0):
            print "A list of all the videos that are NOT High Defination(HD):"
            for s in stringUtil.toTableStringsList(table, ["series title", "season", "episode", "video resolution"]):
                if (( (index % 2) == 0) and (not s.startswith("-"))):
                    s = __colorizeBackground(s)
                print s
                index += 1
        else:
            print "There was no files found that were not High Defination(HD).\n"
        print ""
        index = 1
        if (len(tableHD) > 0):
            print "A list of all the videos that are High Defination(HD):"
            for s in stringUtil.toTableStringsList(tableHD, ["series title", "season", "episode", "video resolution"]):
                if (( (index % 2) == 0) and (not s.startswith("-"))):
                    s = __colorizeBackground(s)
                print s
                index += 1
        else:
            print "There was no files found that were High Defination(HD)."


    except KeyboardInterrupt:
        print ""
        message =  "This script will exit since control-c was executed by end user."
        logging.getLogger(MAIN_LOGGER_NAME).error(message)
        sys.exit(1)
    # #######################################################################
    # Exit the application with zero exit code since we cleanly exited.
    # #######################################################################
    sys.exit()
