#!/usr/bin/env python
"""
This script will install all the required files on the host.

@author    :  Shane Bradley
@contact   :  sbradley@redhat.com
@version   :  0.05
@copyright :  GPLv2
"""
import sys
import os
import os.path
import logging
import logging.handlers
from optparse import OptionParser, Option, SUPPRESS_HELP
import time
import platform
import shutil
# #####################################################################
# Global vars:
# #####################################################################
VERSION_NUMBER = "0.05-3"
MAIN_LOGGER_NAME = "Configs_Installer"
FILES_TO_INSTALL_MAP = {"bash/.bash_profile": os.path.join(os.getenv("HOME"), ".bash_profile"),
                        "bash/.bashrc": os.path.join(os.getenv("HOME"), ".bashrc"),
                        "bash/.aliases.all": os.path.join(os.getenv("HOME"), ".aliases.all"),
                        "bash/.aliases.devel": os.path.join(os.getenv("HOME"), ".aliases.devel"),
                        "bash/.functions.sh": os.path.join(os.getenv("HOME"), ".functions.sh")}

FILES_TO_CREATE = [os.path.join(os.getenv("HOME"), ".bash_profile.priv"),
                   os.path.join(os.getenv("HOME"), ".bashrc.priv")]

# ##############################################################################
# Functions for files and directories
# ##############################################################################
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

def copyFile(pathToSrcFile, pathToDstFile):
    """
    This function will copy a src file to dst file.

    @return: Returns True if the file was copied successfully.
    @rtype: Boolean

    @param pathToSrcFile: The path to the source file that will be copied.
    @type pathToSrcFile: String
    @param pathToDstFile: The path to the destination of the file.
    @type pathToDstFile: String
    """
    if(not os.path.exists(pathToSrcFile)):
        message = "The file does not exist with the path: %s." %(pathToSrcFile)
        logging.getLogger(MAIN_LOGGER_NAME).error(message)
        return False
    elif (not os.path.isfile(pathToSrcFile)):
        message = "The path to the source file is not a regular file: %s." %(pathToSrcFile)
        logging.getLogger(MAIN_LOGGER_NAME).error(message)
        return False
    elif (pathToSrcFile == pathToDstFile):
        message = "The path to the source file and path to destination file cannot be the same: %s." %(pathToDstFile)
        logging.getLogger(MAIN_LOGGER_NAME).error(message)
        return False
    else:
        # Create the directory structure if it does not exist.
        (head, tail) = os.path.split(pathToDstFile)
        if (not mkdirs(head)) :
            # The path to the directory was not created so file
            # could not be copied.
            return False
        # Copy the file to the dst path.
        try:
            shutil.copy(pathToSrcFile, pathToDstFile)
        except shutil.Error:
            message = "Cannot copy the file %s to %s." %(pathToSrcFile, pathToDstFile)
            logging.getLogger(MAIN_LOGGER_NAME).error(message)
            return False
        except OSError:
            message = "Cannot copy the file %s to %s." %(pathToSrcFile, pathToDstFile)
            logging.getLogger(MAIN_LOGGER_NAME).error(message)
            return False
        except IOError:
            message = "Cannot copy the file %s to %s." %(pathToSrcFile, pathToDstFile)
            logging.getLogger(MAIN_LOGGER_NAME).error(message)
            return False
        return (os.path.exists(pathToDstFile))

def copyDirectory(pathToSrcDir, pathToDstDir):
    """
    This function will copy a src dir to dst dir.

    @return: Returns True if the dir was copied successfully.
    @rtype: Boolean

    @param pathToSrcDir: The path to the source dir that will be copied.
    @type pathToSrcDir: String
    @param pathToDstDir: The path to the destination of the dir.
    @type pathToDstDir: String
    """
    if(not os.path.exists(pathToSrcDir)):
        message = "The directory does not exist with the path: %s." %(pathToSrcDir)
        logging.getLogger(MAIN_LOGGER_NAME).error(message)
        return False
    elif (not os.path.isdir(pathToSrcDir)):
        message = "The path to the source directory is not a directory: %s." %(pathToSrcDir)
        logging.getLogger(MAIN_LOGGER_NAME).error(message)
        return False
    elif (pathToSrcDir == pathToDstDir):
        message = "The path to the source directory and path to destination directory cannot be the same: %s." %(pathToDstDir)
        logging.getLogger(MAIN_LOGGER_NAME).error(message)
        return False
    else:
        if (not mkdirs(pathToDstDir)) :
            # The path to the directory was not created so file
            # could not be copied.
            return False
        # Copy the file to the dst path.
        dst = os.path.join(pathToDstDir, os.path.basename(pathToSrcDir))
        try:
            shutil.copytree(pathToSrcDir, dst)
        except shutil.Error:
            message = "Cannot copy the directory %s to %s." %(pathToSrcDir, dst)
            logging.getLogger(MAIN_LOGGER_NAME).error(message)
            return False
        except OSError:
            message = "Cannot copy the directory %s to %s." %(pathToSrcDir, dst)
            logging.getLogger(MAIN_LOGGER_NAME).error(message)
            return False
        except IOError:
            message = "Cannot copy the directory %s to %s." %(pathToSrcDir, dst)
            logging.getLogger(MAIN_LOGGER_NAME).error(message)
            return False
        return (os.path.exists(dst))

def backupDirectory(pathToDSTDir):
    """
    This function will return True if the pathToDSTDir does not exist or the
    directory was successfully rename. If pathToDSTDir exists and was not
    successfully rename then False is returned.

    @return: Returns True if the pathToDSTDir does not exist or the directory
    was successfully rename. If pathToDSTDir exists and was not successfully
    rename then False is returned.
    @rtype: Boolean

    @param pathToDSTDir: The path to the directory that will be backed up.
    @type pathToDSTDir: String
    """
    if (os.path.exists(pathToDSTDir)):
        message = "The path already exists and could contain previous lockdump data: %s" %(pathToDSTDir)
        logging.getLogger(MAIN_LOGGER_NAME).info(message)
        backupIndex = 1
        pathToDST = ""
        keepSearchingForIndex = True
        while (keepSearchingForIndex):
            pathToDST = "%s.bk-%d" %(pathToDSTDir, backupIndex)
            if (os.path.exists(pathToDST)):
                backupIndex += 1
            else:
                keepSearchingForIndex = False
        try:
            message = "The existing output directory will be renamed: %s to %s." %(pathToDSTDir, pathToDST)
            logging.getLogger(MAIN_LOGGER_NAME).status(message)
            shutil.move(pathToDSTDir, pathToDST)
        except shutil.Error:
            message = "There was an error renaming the directory: %s to %s." %(pathToDSTDir, pathToDST)
            logging.getLogger(MAIN_LOGGER_NAME).error(message)
        except OSError:
            message = "There was an error renaming the directory: %s to %s." %(pathToDSTDir, pathToDST)
            logging.getLogger(MAIN_LOGGER_NAME).error(message)
    # The path should not exists now, else there was an error backing up an
    # existing output directory.
    return (not os.path.exists(pathToDSTDir))

# ##############################################################################
# Installation Functions
# ##############################################################################
def install(pathToConfigFiles):
    filesFailedInstall = {}
    if (os.path.isdir(pathToConfigFiles)):
        message = "The files in the following directory will be installed: %s." %(pathToConfigFiles)
        logging.getLogger(MAIN_LOGGER_NAME).info(message)
        keys = FILES_TO_INSTALL_MAP.keys()
        keys.sort()
        for key in keys:
            pathToSrcFile = os.path.join(pathToConfigFiles, key)
            pathToDstFile = FILES_TO_INSTALL_MAP.get(key)
            message = "Copying the file %s to %s." %(pathToSrcFile, pathToDstFile)
            logging.getLogger(MAIN_LOGGER_NAME).debug(message)
            result = copyFile(pathToSrcFile, pathToDstFile)
            if (not result):
                filesFailedInstall[pathToDstFile] = pathToSrcFile

        # Copy the directories"
        print "\n\nCOPY THE DIRECTOIRES."

        # Create some empty files
        print "CREATE THE EMPTY FILE FUNCTION.\n\n"
        for pathToNewFile in FILES_TO_CREATE:
            continue
    else:
        filesFailedInstall.append(pathToConfigFiles)
        message = "The path to the configuration files is invalid: %s" %(pathToConfigFiles)
        logging.getLogger(MAIN_LOGGER_NAME).error(message)
    return (not len(filesFailedInstall) > 0)

# ##############################################################################
# Misc Functions
# ##############################################################################
def exitScript(errorCode=0):
    """
    This function will cause the script to exit or quit. It will return an error
    code and a message.

    @param errorCode: The exit code that will be returned. The default value is 0.
    @type errorCode: Int
    """
    if (removePidFile):
        removePIDFile()
    message = "The script will exit."
    logging.getLogger(MAIN_LOGGER_NAME).info(message)
    sys.exit(errorCode)
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
    cmdParser.add_option("-p", "--path_to_configs",
                         action="store",
                         dest="pathToConfigFiles",
                         help="path to the root directory for the configuration files",
                         type="string",
                         default=os.path.join(os.getenv("HOME"), "github/dot.config"))

    #cmdParser.add_option("-o", "--options",
    #                     action="extend",
    #                     dest="options",
    #                     help="",
    #                     type="string",
    #                     default=[])
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

        commandDescription  ="%s will install the configuration files and scripts to the host.\n"%(self.__commandName)

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

        message = "Installing the configuration files will begin."
        logging.getLogger(MAIN_LOGGER_NAME).info(message)
        # #######################################################################
        # Verify they want to continue because this script will trigger sysrq events.
        # #######################################################################
        if (not cmdLineOpts.disableQuestions):
            valid = {"yes":True, "y":True, "no":False, "n":False}
            question = "Are you sure you want to install the configuration files and scripts to this host?"
            prompt = " [y/n] "
            while True:
                sys.stdout.write(question + prompt)
                choice = raw_input().lower()
                if (choice in valid):
                    if (valid.get(choice)):
                        # If yes, or y then exit loop and continue.
                        break
                    else:
                        message = "The script will not continue since you chose not to continue."
                        logging.getLogger(MAIN_LOGGER_NAME).error(message)
                        exitScript(removePidFile=True, errorCode=1)
                else:
                    sys.stdout.write("Please respond with '(y)es' or '(n)o'.\n")
        # Install the configuration files
        errorCode = 0
        if (install(cmdLineOpts.pathToConfigFiles)):
            message = "The installation was successful."
            logging.getLogger(MAIN_LOGGER_NAME).info(message)
        else:
            errorCode = 1
            message = "The installation was unsuccessful. There was errors detected during the installation of the files."
            logging.getLogger(MAIN_LOGGER_NAME).error(message)
        # Exit the script.
        message = "The script has completed."
        logging.getLogger(MAIN_LOGGER_NAME).info(message)
        exitScript(errorCode)
    except KeyboardInterrupt:
        message =  "This script will exit since control-c was executed by end user."
        sys.exit(2)
    except Exception, e:
        message = "An error occurred and the script will exit."
        sys.exit(1);
    # #######################################################################
        # Exit the application with zero exit code since we cleanly exited.
    # #######################################################################
    sys.exit()
