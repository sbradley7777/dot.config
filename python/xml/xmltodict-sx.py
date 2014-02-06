#!/usr/bin/env python
"""
I found this code at: http://code.activestate.com/recipes/410469-xml-as-dictionary/
@author   :  Shane Bradley
@contact  :  sbradley@redhat.com
@version  :  1.00
"""
import sys
import os.path
from sxstrata import XmlToDict


def printXML(pathToXmlFile) :
    if (not os.path.isfile(pathToXmlFile)):
        print "Error: The xml file does not exist: %s" %(pathToXmlFile)
        sys.exit()
    # ##########################################################################
    # If file exists then start parsing.
    # ##########################################################################
    xmlString = ""
    try:
        f = open(pathToXmlFile, "r")
        lines = f.readlines()
        f.close()
        for line in lines:
            xmlString += "%s" %(line)
    except IOError:
        print "Error: There was an error reading the file: %s." %(pathToXmlFile)

    #print xmlString
    #xmlDict = XmlToDict().xmlFileToDict(pathToXmlFile)
    xmlDict = XmlToDict().xmlToDict(xmlString)

    for key in  xmlDict.keys():
        for fileList in xmlDict[key]:
            print "ITEM: %s" %(str(fileList))
            print


################################################################################
#Run script
################################################################################
if __name__ == "__main__":
    pathToXmlFile = "cluster.conf"
    printXML(pathToXmlFile)
    print "---------------------------"
    print ""
    pathToXmlFile = "example.xml"
    printXML(pathToXmlFile)
    sys.exit()
################################################################################



