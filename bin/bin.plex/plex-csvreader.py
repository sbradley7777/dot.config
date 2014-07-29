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
import logging
import logging.handlers
import csv

import textwrap
import string
# #####################################################################
# Global vars:
# #####################################################################
PATH_TO_FILENAME = "/Users/sbradley/Desktop/Plex2csv/Documentaries-Extreme-20140728-121721.csv"

VERSION_NUMBER = "0.9-8"
MAIN_LOGGER_NAME = "%s" %(os.path.basename(sys.argv[0]))


class StringUtil:
    def wrapParagraph(s, width=98, newline=True):
        rString = textwrap.fill(s, width=98).rstrip()
        if (newline):
            return "%s\n" %(rString)
        return rString
    wrapParagraph = staticmethod(wrapParagraph)

    def wrapParagraphURLs(s, urls, width=98, newline=True):
        rString = StringUtil.wrapParagraph(s, width, newline=False).rstrip()
        for url in urls:
            rString += "\n- %s" %(url)
        if (newline):
            return "%s\n" %(rString)
        return rString
    wrapParagraphURLs = staticmethod(wrapParagraphURLs)

    def formatBulletString(description, urls, tableOfStrings=None, indentChar="*", indentSize=3, width=98) :
        # Orginal width was 65.
        # Only the first character will be used for the bullet. If no
        # character is passed then a whitespace will be used.
        if (len(indentChar) > 1):
            indentChar = indentChar[:1]
        elif (len(indentChar) <= 0):
            indentChar = " "
        initIndent = indentChar
        # Add in the whitespaces that will finish the indents
        if (not len(initIndent) >= indentSize):
            initIndent += " " * (indentSize - len(initIndent))
        # Create the subsequent intent size which will be all
        # whitespaces.
        subIndent = " " * indentSize

        # Format the string with textwrap
        rString = "\n".join(textwrap.wrap(description, width=width, initial_indent=initIndent, subsequent_indent=subIndent))
        rString += "\n"
        # Append the urls to the return string
        if (not urls == None):
            for url in urls:
                rString += "%s - %s\n" %(subIndent, url)
            rString = rString.strip("\n")
            rString += "\n"
        # Add the table string if not None
        if (not tableOfStrings == None):
            rString += "\n"
            for s in tableOfStrings:
                rString += "%s%s\n" %(subIndent, s)
            rString += "\n"
        return rString
    formatBulletString = staticmethod(formatBulletString)

    # #######################################################################
    # Functions for creating a formatted table from lists of lists:
    # #######################################################################
    def __formatTableValue(self, tableValue):
        """
        Format a number or strings according to given places.
        Adds commas and will truncate floats into ints.

        If a string is the parameter then execption will be caught and
        string will be returned.

        @return: Returns a formatted string.
        @rtype: String

        @param tableValue: The value that will be formatted.
        @type tableValue: Int or String
        """
        import locale
        locale.setlocale(locale.LC_NUMERIC, "")

        try:
            inum = int(tableValue)
            return locale.format("%.*f", (0, inum), True)
        except (ValueError, TypeError):
            return str(tableValue)

    def __getMaxColumnWidth(self, table, index):
        """
        Get the maximum width of the given column index from the
        current table. If index is out of range then a -1 is returned.

        @return: Returns the max width for that index or column.
        @rtype: Int

        @param table: A array of arrays(Can be strings, ints, floats).
        @type table: Array
        @param index: The current index of what will be compared.
        @type index: Int
        """
        try:
            return max([len(self.__formatTableValue(row[index])) for row in table])
        except IndexError:
            return -1

    def formatStringListsToTable(self, table, headerList=None):
        """
        This function will take an array of arrays and then output a
        single string that is a formatted table.

        An empty list will be returned if the column count is not the
        same for each row in the table.

        I got code from this url and modified it:
        http://ginstrom.com/scribbles/2.15/09/04/pretty-printing-a-table-in-python/

        Example(added spacing to make example clear):
        table = [["",       "names", "birthyear", "age"], ["NCuser", "bob",   1976,         35]]

        @return: Returns a list of strings(row of strings) that has
        the proper spacing in each column.
        @rtype: String

        @param table: A array of arrays(Can be strings, ints, floats).
        @type table: Array
        @param headerList: A list of strings that will be the headers
        for each column in the table.
        @type headerList: Array
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
                    tableStrings.append(str(self.__formatTableValue(row[i]).ljust(col_paddings[i] + 2)))
                except IndexError:
                    continue
            tableStringsList.append(tableStrings)
        return tableStringsList

    def toTableString(self, table, headerList=None):
        """
        This function will take an array of arrays and then output a
        single string that is a formatted table.

        An empty string will be returned if the column count is not the
        same for each row in the table.

        @return: Returns a formatted table string with correct spacing
        for each column.
        @rtype: String

        @param table: A array of arrays(Can be strings, ints, floats).
        @type table: Array
        @param headerList: A list of strings that will be the headers
        for each column in the table.
        @type headerList: Array
        """
        if (not len(table) > 0):
            return ""
        tString = ""
        tableStringsList = self.formatStringListsToTable(table, headerList)
        for tableStrings in tableStringsList:
            currentLine = ""
            for ts in tableStrings:
                currentLine += ts
            if (not currentLine.startswith("-")):
                tString += "%s\n" %(currentLine)
        return tString.rstrip()

def __create_logger():
    # #######################################################################
    # Setup the logger and create config directory
    # #######################################################################
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

def dequote(s):
    """
    If a string has single or double quotes around it, remove them.
    If a matching pair of quotes is not found, return the string unchanged.
    """
    if (s.startswith(("'", '"')) and s.endswith(("'", '"')) and (s[0] == s[-1])):
        s = s[1:-1]
    return s
# ###############################################################################
# Main Function
# ###############################################################################
if __name__ == "__main__":
    try:
        # Create the logger
        __create_logger()
        message = "The parsing of the csv will start."
        logging.getLogger(MAIN_LOGGER_NAME).info(message)

        if (not os.path.exists(PATH_TO_FILENAME)):
            message = "The path to the csv file does not exists: %s." %(PATH_TO_FILENAME)
            logging.getLogger(MAIN_LOGGER_NAME).error(message)
            sys.exit(1)

        data = []
        try:
            csv_file = open(PATH_TO_FILENAME, "r")
            data = csv_file.readlines()
        except IOError:
            message = "There was an error reading the file: %s." %(PATH_TO_FILENAME)
            logging.getLogger(MAIN_LOGGER_NAME).error(message)
            sys.exit(1)

        csv_fields = []
        if (not len(data) > 0):
            message = "There was no data to parse for file: %s." %(PATH_TO_FILENAME)
            logging.getLogger(MAIN_LOGGER_NAME).error(message)
            sys.exit(1)
        else:
            for s in data.pop(0).split(","):
                csv_fields.append(dequote(s))

        if (not len(csv_fields) > 0):
            message = "The field attribute names were not found."
            logging.getLogger(MAIN_LOGGER_NAME).warning(message)

        message = "There was %d records in the csv file." %(len(data))
        logging.getLogger(MAIN_LOGGER_NAME).info(message)


        table = []
        dict_reader = csv.DictReader(data, fieldnames = csv_fields, delimiter = ',', quotechar = '"')
        for row in dict_reader:
            series_title = row.get("Series Title").replace("\n", "")
            season = row.get("Season")
            episode = row.get("Episode")
            video_resolution = row.get("Video Resolution")
            table.append([series_title, season, episode, video_resolution])
        sutil = StringUtil()
        stable = sutil.toTableString(table, ["series title", "season", "episode", "video resolution"])
        print stable
    except KeyboardInterrupt:
        print ""
        message =  "This script will exit since control-c was executed by end user."
        logging.getLogger(MAIN_LOGGER_NAME).error(message)
        sys.exit(1)
    # #######################################################################
    # Exit the application with zero exit code since we cleanly exited.
    # #######################################################################
    sys.exit()
