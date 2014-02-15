Video Latent Scene Attributes (VLSA) System
===========================================

Main repository for VLSA code

To compile from the command line
================================

Run this line once:

	./install_jars.sh

Then, every time you need to compile the project:

	mvn compile

Test the project using:

    mvn test

To import in IntelliJ IDEA
==========================

Install IntelliJ IDEA
---------------------

If you did not do it already, download IntelliJ IDEA Community Edition from here: http://www.jetbrains.com/idea/free_java_ide.html. Install the Scala plugin. See this page for installation details: http://confluence.jetbrains.com/display/SCA/Getting+Started+with+IntelliJ+IDEA+Scala+Plugin

Import vlsa
-----------

When you are done you are ready to import vlsa. Go to File/Import Project. In the file window that opens select vlsa/pom.xml. In the next window, make sure that only the following options are checked:

	Search for projects recursively
	Project format: .idea (directory based)
	Import Maven projects automatically
	Create IDEA modules for aggregator projects (with 'pom' packaging)
	Create module groups for multi-module Maven projects
	Keep source and test folders in reimport
	Exclude build directory (%PROJECT_ROOT%/target)
	Use Maven output directories
	Generated source folders: Detect automatically
	Phase to be used for folders update: process-resources
	Automatically download: Sources (YES) Documentation (YES)

Click Next. In the second window, check the Maven project edu.arizona.sista:vlsa. Click Next. In the next window select SDK 1.6 (should be already selected). Click Next. In the final window, make sure the project name is vlsa. You can also choose the project directory location on your disk here. Click Finish. The result should be an IntelliJ project called vlsa.

Before using vlsa, please do the following:
* First, increase the memory size available to IDEA. For this, edit the idea.vmoptions file. See this page for details: http://stackoverflow.com/questions/2435569/how-to-give-more-memory-to-intellij-idea-9-11.
* Second, edit the code style for both Java and Scala programs to use an indentation of two spaces. For this, go to Preferences (or Settings in Windows), then Code Style/General. Set Tab Size, Indent, and Continuation Indent to 2. Uncheck Use Tab Character. Do the same in the Scala and Java tabs under Code Style. 

Compile and test
----------------

After this is done, compile the project. It should compile without errors (maybe a few warnings). If you see this warning:

	Unregistered Git root detected

click the popup to go to the configuration window and accept the proposed configuration with the new Git root (click Ok). 

Once the project has compiled run a test program to double check. For example this:

	src/test/scala/edu.arizona.sista.vlsa.models.evaluation.TestEvaluation

To run, right click on the file, and choose the Run option. You may need to give the programs sufficient memory to run. To adjust find Edit Configurations, then add -Xmx3g in the VM Options field.

