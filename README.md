# SML/NJ Legacy

This project is the old version of Standard ML of New Jersey
that continues to support older systems (*e.g.*, 32-bit machines).
We will continue to fix bugs and make minor improvements while
the main development branch matures.  We will also backport
additions to the **SML/NJ Library** and other components.

## Building from Source

The preferred way to build the system is to follow the installation
instructions for your platform.  If, however, you want to compile the
current source from **GitHub**, the following steps should usually work.

1. Start with a fresh clone of the repository (let `ROOT` be the
   root directory of the clone.

    ``` sh
    % git clone git@github.com:smlnj/legacy.git $ROOT
    ```

2. Configure and install the corresponding release in the `$ROOT`:

    ``` sh
    % cd $ROOT
    % config/install.sh
    ```

3. Use this compiler to recompile from sources

    ``` sh
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
