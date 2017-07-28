#!/usr/bin/python
"""
This script will check if a case is closed in the Red Hat portal. If the
case is closed then it will remove the locally stored files for that case.

* The script uses the tool "redhat-support-tool" to access the cases on
  the Red Hat portal.
* The script requires has option to enable "sudo rm -rf <dirname>" if
  normal user cannot remove the directory.

@author    : Shane Bradley
@contact   : sbradley@redhat.com
@version   : 0.1
@copyright : GPLv2

Example:
$ ~/bin/bin.priv/purge_data-rh.py -p ~/attachments/
$ ~/bin/bin.priv/purge_data-rh.py -p ~/sxarchive/

"""
import sys
import logging
import logging.handlers
import os
import os.path
from optparse import OptionParser, Option, SUPPRESS_HELP
import subprocess
import shutil

# #####################################################################
# Global vars:
# #####################################################################
VERSION_NUMBER = "0.1-1"
MAIN_LOGGER_NAME = "%s" %(os.path.basename(sys.argv[0]))

# #####################################################################
# Helper File Functions
# ####################################################################
def run_command(path_to_command, options) :
    if (not os.path.exists(path_to_command)):
        message = "The command does not exist: %s" %(path_to_command)
        logging.getLogger(MAIN_LOGGER_NAME).error(message)
        return None
    command = [path_to_command] + options
    #task = subprocess.Popen(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    task = subprocess.Popen(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    (stdout, stderr) = task.communicate()
    return stdout.splitlines()

# ##############################################################################
# Get user selected options
# ##############################################################################
def __get_options(version) :
    cmd_parser = OptionParserExtended(version)
    cmd_parser.add_option("-d", "--debug",
                         action="store_true",
                         dest="enableDebugLogging",
                         help="enables debug logging",
                         default=False)
    cmd_parser.add_option("-q", "--quiet",
                         action="store_true",
                         dest="disableLoggingToConsole",
                         help="disables logging to console",
                         default=False)
    cmd_parser.add_option("-S", "--enable_sudo_rm",
                         action="store_true",
                         dest="enable_sudo_rm",
                          help="a directory will be removed with as root (sudo rm -rf)",
                         default=False)
    cmd_parser.add_option("-p", "--path_to_attachments_dir",
                         action="store",
                         dest="path_to_attachments_dir",
                         help="the path to the attachments directory",
                         type="string",
                         metavar="<input filename>",
                         default="")
    # Get the options and return the result.
    (cmdLine_opts, cmdLine_args) = cmd_parser.parse_args()
    return (cmdLine_opts, cmdLine_args)

# ##############################################################################
# OptParse classes for commandline options
# ##############################################################################
class OptionParserExtended(OptionParser):
    def __init__(self, version) :
        self.__command_name = os.path.basename(sys.argv[0])
        OptionParser.__init__(self, option_class=ExtendOption,
                              version="%s %s\n" %(self.__command_name, version),
                              description="%s \n"%(self.__command_name))

    def print_help(self):
        self.print_version()
        examples_message = "\n"
        OptionParser.print_help(self)
        #print examples_message

class ExtendOption (Option):
    ACTIONS = Option.ACTIONS + ("extend",)
    STORE_ACTIONS = Option.STORE_ACTIONS + ("extend",)
    TYPED_ACTIONS = Option.TYPED_ACTIONS + ("extend",)

    def take_action(self, action, dest, opt, value, values, parser):
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
        (cmdline_opts, cmdline_args) = __get_options(VERSION_NUMBER)
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
        syslog_handler = logging.handlers.SysLogHandler(address = '/dev/log')
        syslog_handler.setFormatter(logging.Formatter("%(name)s: %(message)s"))
        logger.addHandler(syslog_handler)
        # logger.removeHandler(sysLogHandler)

        # Create a function for the STATUS_LEVEL since not defined by python. This
        # means you can call it like the other predefined message
        # functions. Example: logging.getLogger("loggerName").status(message)
        setattr(logger, "status", lambda *args: logger.log(logging.STATUS, *args))
        stream_handler = logging.StreamHandler()
        stream_handler.setLevel(logLevel)
        stream_handler.setFormatter(logging.Formatter("%(levelname)s %(message)s"))
        logger.addHandler(stream_handler)

        # #######################################################################
        # Set the logging levels.
        # #######################################################################
        if ((cmdline_opts.enableDebugLogging) and (not cmdline_opts.disableLoggingToConsole)):
            logging.getLogger(MAIN_LOGGER_NAME).setLevel(logging.DEBUG)
            stream_handler.setLevel(logging.DEBUG)
            message = "Debugging has been enabled."
            logging.getLogger(MAIN_LOGGER_NAME).debug(message)
        if (cmdline_opts.disableLoggingToConsole):
            stream_handler.setLevel(logging.CRITICAL)

        # #######################################################################
        # Run main
        # #######################################################################
        if (not cmdline_opts.path_to_attachments_dir):
            message = "The option -p is required for path to the data directory."
            logging.getLogger(MAIN_LOGGER_NAME).error(message)
            sys.exit(1)
        if (not os.path.exists(cmdline_opts.path_to_attachments_dir)):
            message = "The path to the data directory does not exist: %s." %(cmdline_opts.path_to_attachments_dir)
            logging.getLogger(MAIN_LOGGER_NAME).error(message)
            sys.exit(1)

        logger.info("Analyzing data from the directory: %s." %(cmdline_opts.path_to_attachments_dir))
        case_numbers = []
        for dirname in os.listdir(cmdline_opts.path_to_attachments_dir):
           if (dirname.isdigit()):
               case_numbers.append(dirname)

        closed_case_numbers = []
        case_numbers.sort()
        index = 1
        for case_number in case_numbers:
            command_options =  ["getcase", case_number]
            result = run_command("/usr/bin/redhat-support-tool", command_options)
            for line in result:
                if (line.startswith("Status:       ")):
                    status = line.split("Status:")[-1].strip()
                    message = "(%03d/%03d) %s | STATUS: %s" %(index, len(case_numbers), case_number, status)
                    logging.getLogger(MAIN_LOGGER_NAME).debug(message)
                    if (status.lower() == "closed"):
                        closed_case_numbers.append(case_number)
                        break
            index += 1

        logger.info("Purging %d sub-directories from the directory: %s." %(len(closed_case_numbers), cmdline_opts.path_to_attachments_dir))
        index = 1
        for case_number in closed_case_numbers:
            path_to_dir = os.path.join(cmdline_opts.path_to_attachments_dir, case_number)
            if (os.path.exists(path_to_dir)):
                message = "(%03d/%03d) Removing the data for the case: %s." %(index, len(closed_case_numbers), case_number)
                logging.getLogger(MAIN_LOGGER_NAME).info(message)
                try:
                    shutil.rmtree(path_to_dir)
                except OSError:
                    message = "An error occurred removing the directory: %s" %(path_to_dir)
                    logging.getLogger(MAIN_LOGGER_NAME).debug(message)
                    if (cmdline_opts.enable_sudo_rm):
                        message = "Removing the directory as root with sudo since normal user cannot remove."
                        logging.getLogger(MAIN_LOGGER_NAME).debug(message)

                        # Add ability if there is error then try to remove manually
                        # by running sudo. Add option to enable sudo deletion.
                        run_command("/usr/bin/sudo", ["rm", "-rf", path_to_dir])

                        # Not sure how to do error checking unless i mod
                        # run_command() to return tuple.
            index += 1
    except KeyboardInterrupt:
        print ""
        message =  "This script will exit since control-c was executed by end user."
        logging.getLogger(MAIN_LOGGER_NAME).error(message)
        sys.exit(1)
    # #######################################################################
    # Exit the application with zero exit code since we cleanly exited.
    # #######################################################################
    sys.exit()
