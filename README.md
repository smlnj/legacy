# SML/NJ Legacy

This project is the old version of Standard ML of New Jersey
that continues to support older systems (*e.g.*, 32-bit machines).
We will continue to fix bugs and make minor improvements while
the main development branch matures.  We will also backport
additions to the **SML/NJ Library** and other components.

The latest legacy version is [110.99.8](https://smlnj.org/dist/working/110.99.8),
which was released on April 25, 2025.

## Documentation

- The [**SML/NJ** web site](https://smlnj.org)

- The [**SML Basis Library**](https://smlfamily.github.io/Basis/index.html)
  Specification of the **SML '97 Basis Library**, which **SML/NJ** supports

- The [**SML of NJ Library**](https://www.smlnj.org/doc/smlnj-lib/index.html)
  Documentation for the additional libraries included in **SML/NJ**

- The [Standard ML of New Jersey Wiki](https://github.com/smlnj/.github/wiki)
  This wiki documents the implementation of **SML/NJ**

## Installation

**SML/NJ** is available in several ways

* we distribute installer packages for **macOS** and **Windows**

* it is available from many package managers for **Linux**, **macOS**,
  and **Windows**.

* you can build it from the distribution files using the `config/install.sh`
  script

* you can build it from the **GitHub** repository

### Linux

Note that for the latest version of **SML/NJ**, you should build from source
(as described below), since the versions available from **Linux** package managers
are often out of date.

| Package Manager | Installation Command |
| --- | --- |
| [![Arch Linux package](https://img.shields.io/archlinux/v/extra/x86_64/smlnj?logo=archlinux&label=arch)](https://archlinux.org/packages/extra/x86_64/smlnj/) | `pacman -S smlnj` |
| [![Debian Unstable](https://img.shields.io/debian/v/smlnj?logo=debian)](https://packages.debian.org/sid/smlnj) | `apt install smlnj` |
| [![Gentoo](https://img.shields.io/badge/gentoo-110.99.6.1-blue?logo=gentoo)](https://packages.gentoo.org/packages/dev-lang/smlnj) | `emerge --ask dev-lang/smlnj` |
| [![Nixpkgs Unstable](https://img.shields.io/badge/nixpkgs-110.99.7.1-blue?logo=nixos)](https://search.nixos.org/packages?channel=unstable&show=smlnj&from=0&size=1&sort=relevance&type=packages&query=smlnj) | `nix-shell -p smlnj` |
| [![Ubuntu Unstable](https://img.shields.io/ubuntu/v/smlnj?logo=ubuntu)](https://launchpad.net/ubuntu/plucky/+package/smlnj) | `apt install smlnj` |
| [![openSUSE](https://img.shields.io/badge/opensuse-110.99.5-blue?logo=suse)](https://software.opensuse.org/package/smlnj) | `zypper install smlnj` |

### MacOS

We distribute installer packages for both **macOS** on both
[32-bit](https://smlnj.org/dist/working/110.99.8/smlnj-x86-110.99.8.pkg) and
and [64-bit](https://smlnj.org/dist/working/110.99.8/smlnj-amd64-110.99.8.pkg) Intel hardware
(for an Arm version, see the [development version](https://github.com/smlnj/smlnj).
It is also available from [Homebrew](https://brew.sh).

| Package Manager | Installation Command |
| --- | --- |
| [![Homebrew](https://img.shields.io/badge/homebrew-110.99.8-blue?logo=homebrew)](https://formulae.brew.sh/cask/smlnj) | `brew install --cask smlnj` |

### Windows

We distribute a
[32-bit MSI package](https://smlnj.org/dist/working/110.99.8/smlnj-110.99.8.msi)
for **Windows**, but it is also available via standard package managers.

| Package Manager | Installation Command |
| --- | --- |
| [![WinGet](https://img.shields.io/winget/v/SMLNJ.SMLNJ.Legacy?logo=winget)](https://winstall.app/apps/SMLNJ.SMLNJ.Legacy) | `winget install -e --id SMLNJ.SMLNJ.Legacy` |
| [![Chocolatey](https://img.shields.io/chocolatey/v/smlnj?logo=chocolatey)](https://community.chocolatey.org/packages/smlnj) | `choco install smlnj` |

## Building from the Distribution Files

On **Linux**, **macOS**, and other **Unix**-like platforms, it is possible to
build the system from the distributed tar files.  Let `VERSION` be the version
that you want to install (*e.g.*, `110.99.8`).

1. Start by creating an installation directory

    ``` console
    % mkdir smlnj
    % cd smlnj
    ```

2. Download the `config.tgz` file for the version that you want to install
   using **curl** (or **wget**).

    ``` console
    % curl -O https://smlnj.org/dist/working/$VERSION/config.tgz
    ```

3. Unpack the `config.tgz` file.

    ``` console
    % tar -xzf config.tgz
    ```

4. Run the installation script

    ``` console
    % config/install.sh -default $SIZE
    ```
  where `SIZE` is either 32 or 64.  Note that the **x86-64** (aka **amd64**)
  is the only 64-bit architecture supported by the legacy version of **SML/NJ**.


## Building from the **GitHub** Repository

The preferred way to build the system is to follow the installation
instructions for your platform.  If, however, you want to compile the
current source from **GitHub**, the following steps should usually work.

1. Start with a fresh clone of the repository (let `ROOT` be the
   root directory of the clone.

    ``` console
    % git clone git@github.com:smlnj/legacy.git $ROOT
    ```

2. Configure and install the corresponding release in the `$ROOT`:

    ``` console
    % cd $ROOT
    % config/install.sh
    ```

3. Use this compiler to recompile from sources

    ``` console
    % cd base/system
    % ./cmb-make ../../bin/sml
    % ./makeml
    ```

    The "`../bin/sml`" argument is optional; if omitted, then the `sml` command in the
    user's `PATH` will be used.  Once can also specify a different path to an `sml`
    command, when appropriate.

    Also note that because of a flaw in the way that **CM** handles conditionals in CM
    files, it is necessary that the **ml-yacc** and **ml-ulex** commands be available
    in the `PATH`.  If you do not have an installation of **SML/NJ** available, then use
    the following steps:

    ``` sh
    % cd base/system
    % PATH=$PATH:$PWD/../../bin ./cmb-make ../../bin/sml
    % ./makeml
    ```

4. At this point, you can test the compiled code while in the
   `system` directory.

    ``` sh
    % ./testml
    ```

5. To finish the build process, you have to install the new compiler
   heap image and rebuild the libraries.

    ``` sh
    % ./installml -clean
    % cd ../..
    % config/install.sh
    ```

You should now have a version of the system in `$ROOT/bin/sml` that
corresponds to the latest version of the source on **GitHub**.
If you subsequently pull changes from the repository, you can rebuild
starting at Step 3.

These instructions are for Unix-like systems (including **macOS**).  We do not
have scripts (*e.g.*, `makeml`) to support this process on **Windows**.
