#!/bin/bash

# When run this will invoke maven and install our library files
# this script will need to be invoked on addition of new jars
# or if you do a clean checkout on a new system!
# Only put jars that cant' be found via Maven in here!

mvn install:install-file -DartifactId=jollyday -DgroupId=de.jollyday -Dversion=0.4.7 -Dpackaging=jar -Dfile=lib/jollyday.jar

mvn install:install-file -DartifactId=jaws-bin -DgroupId=edu.smu.tspell.wordnet -Dversion=1.3 -Dpackaging=jar -Dsources=lib/jaws-sources.jar -Dfile=lib/jaws-bin.jar
mvn install:install-file -DartifactId=jawjaw -DgroupId=edu.cmu.lti -Dversion=1.0.2 -Dpackaging=jar -Dfile=lib/jawjaw-1.0.2.jar
mvn install:install-file -DartifactId=ws4j -DgroupId=edu.cmu.lti -Dversion=1.0.1 -Dpackaging=jar -Dsources=lib/ws4j-1.0.1-sources.jar -Dfile=lib/ws4j-1.0.1.jar
