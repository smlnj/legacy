# Building a Windows MSI

This directory contains the infrastructure for building a Windows
MSI installer for SML/NJ.  The process is split into an pre-flight
script, which is run under either WSL or Cygwin, and the build script,
which is run in the Windows shell.

## Instructions

### Step 1 -- Build a Fresh Source Tree

The first step is executed using a Unix shell (either WSL or Cygwin).
We start by running the command

``` shell
  ./fetch-dist.sh $VERSION
```

which will fetch the required files for the specified version
(e.g., 110.99.2) into the directory `win-dist/smlnj` (if the `smlnj`
directory already exists, then the script does nothing).

This script assumes that the distribution files have already been uploaded
to the standard distribution site

	http://smlnj.cs.uchicago.edu/dist/working/$VERSION/

and that the targets file is initialized correctly.

### Step 2 -- Bootstrapping and Building the MSI

The second step builds the runtime system, compiler, and tools,
and then creates the MSI file.

This step should be run in the Visual Studio native x86 shell.
We start by setting the `SMLNJ_HOME` variable to the path to the
`smlnj` directory created in Step 1.

``` bat
  set SMLNJ_HOME=c:\path\to\smlnj
```

Then we run the script to build the MSI in that directory:

``` bat
  chdir %SMLNJ_HOME%
  build-msi.bat
```

It uses the [WiX Tools](https://wixtoolset.org) that can be found
in the `win-dist\WinSetup` directory for this last step.

## WSL Notes

### Filesystem Issues

Windows does not understand the Ubuntu file systems used by WSL, so
you should work in the Windows file system.  The Windows user directories
are found at `/mnt/c/Users`.

### Installing WSL

Instructions for installing Windows Subsytem for Linux can be found at
https://docs.microsoft.com/en-us/windows/wsl/install.

Once installed, you need to run the commands

``` shell
sudo apt update && sudo apt upgrade

sudo apt install subversion
```

(Actually, since the legacy version of SML/NJ is now hosted on
GitHub, you probably do not need subversion).

## Cygwin Notes

TODO
