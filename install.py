#!/usr/bin/env python
"""
This script will install all the required files on the host.

@author    :  Shane Bradley
@contact   :  sbradley@redhat.com
@version   :  0.05
@copyright :  GPLv2
"""
import sys

"""
@cvar VERSION_NUMBER: The current version number of sxconsole.
@type VERSION_NUMBER: String
"""
VERSION_NUMBER = "0.05-1"
MAIN_LOGGER_NAME="Configs_Installer"


# ###############################################################################
# Main Function
# ###############################################################################
if __name__ == "__main__":
    try:
        # #######################################################################
        # Setup the logger.
        # #######################################################################
        message = "Installing the configuration files."


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
