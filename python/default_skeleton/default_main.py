#!/usr/bin/python
"""

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

# #####################################################################
# Global vars:
# #####################################################################
VERSION_NUMBER = "0.1-1"
MAIN_LOGGER_NAME = "%s" %(os.path.basename(sys.argv[0]))

def writeToFile(pathToFilename, data, appendToFile=True, createFile=False):
    """
    This function will write a string to a file.

    @return: Returns True if the string was successfully written to the file,
    otherwise False is returned.
    @rtype: Boolean

    @param pathToFilename: The path to the file that will have a string written
    to it.
    @type pathToFilename: String
    @param data: The string that will be written to the file.
    @type data: String
    @param appendToFile: If True then the data will be appened to the file, if
    False then the data will overwrite the contents of the file.
    @type appendToFile: Boolean
    @param createFile: If True then the file will be created if it does not
    exists, if False then file will not be created if it does not exist
    resulting in no data being written to the file.
    @type createFile: Boolean
    """
    [parentDir, filename] = os.path.split(pathToFilename)
    if (os.path.isfile(pathToFilename) or (os.path.isdir(parentDir) and createFile)):
        try:
            filemode = "w"
            if (appendToFile):
                filemode = "a"
            fout = open(pathToFilename, filemode)
            fout.write(data + "\n")
            fout.close()
            return True
        except UnicodeEncodeError, e:
            message = "There was a unicode encode error writing to the file: %s." %(pathToFilename)
            logging.getLogger(MAIN_LOGGER_NAME).error(message)
            return False
        except IOError:
            message = "There was an error writing to the file: %s." %(pathToFilename)
            logging.getLogger(MAIN_LOGGER_NAME).error(message)
            return False
    return False

def mkdirs(pathToDSTDir):
    """
    This function will attempt to create a directory with the path of the value of pathToDSTDir.

    @return: Returns True if the directory was created or already exists.
    @rtype: Boolean

    @param pathToDSTDir: The path to the directory that will be created.
    @type pathToDSTDir: String
    """
    if (os.path.isdir(pathToDSTDir)):
        return True
    elif ((not os.access(pathToDSTDir, os.F_OK)) and (len(pathToDSTDir) > 0)):
        try:
            os.makedirs(pathToDSTDir)
        except (OSError, os.error):
            message = "Could not create the directory: %s." %(pathToDSTDir)
            logging.getLogger(MAIN_LOGGER_NAME).error(message)
            return False
        except (IOError, os.error):
            message = "Could not create the directory with the path: %s." %(pathToDSTDir)
            logging.getLogger(MAIN_LOGGER_NAME).error(message)
            return False
    return os.path.isdir(pathToDSTDir)


def getDataFromFile(pathToSrcFile) :
    """
    This function will return the data in an array. Where each newline in file
    is a seperate item in the array. This should really just be used on
    relatively small files.

    None is returned if no file is found.

    @return: Returns an array of Strings, where each newline in file is an item
    in the array.
    @rtype: Array

    @param pathToSrcFile: The path to the file which will be read.
    @type pathToSrcFile: String
    """
    if (len(pathToSrcFile) > 0) :
        try:
            fin = open(pathToSrcFile, "r")
            data = fin.readlines()
            fin.close()
            return data
        except (IOError, os.error):
            message = "An error occured reading the file: %s." %(pathToSrcFile)
            logging.getLogger(MAIN_LOGGER_NAME).error(message)
    return None

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
    cmdParser.add_option("-p", "--path_to_filename",
                         action="store",
                         dest="pathToSrc",
                         help="the path to the filename that will be parsed",
                         type="string",
                         metavar="<input filename>",
                         default="")
    cmdParser.add_option("-o", "--path_to_output_filename",
                         action="store",
                         dest="pathToDst",
                         help="the path to the output filename",
                         type="string",
                         metavar="<output filename>",
                         default="")
 # Get the options and return the result.
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

        # Log to main system logger that script has started then close the
        # handler before the other handlers are created.
        sysLogHandler = logging.handlers.SysLogHandler(address = '/dev/log')
        logger.addHandler(sysLogHandler)
        logger.info("Capturing of the data to analyze GFS2 lockdumps.")
        logger.removeHandler(sysLogHandler)

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
        # 
        # #######################################################################
        message = "The script ran."
        logging.getLogger(MAIN_LOGGER_NAME).info(message)
    except KeyboardInterrupt:
        print ""
        message =  "This script will exit since control-c was executed by end user."
        logging.getLogger(MAIN_LOGGER_NAME).error(message)
        sys.exit(1)
    # #######################################################################
    # Exit the application with zero exit code since we cleanly exited.
    # #######################################################################
    sys.exit()
