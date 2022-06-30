This directory contains the infrastructure for building a Windows
MSI installer for SML/NJ.  The process is split into an pre-flight
script, which is run under Cygwin, and the build script, which is run
under Windows.

	./fetch-dist.sh $VERSION

will fetch the required files for the specified version (e.g., 110.97).
This script assumes that the distribution files have already been uploaded
to the standard distribution site

	http://smlnj.cs.uchicago.edu/dist/working/$VERSION/

and that the targets file is initialized correctly.

The second step builds the runtime system, compiler, and tools,
and then creates the MSI file.

    build-msi.bat

It uses the WiX Tools for this last step ().

