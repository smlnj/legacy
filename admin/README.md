== Administration utilities for SML/NJ

This directory contains various scripts to help with the Administration
of the SML/NJ svn repository (and working copies).

  README.md			-- this file

  build-tar-files.sh		-- script to export sources from the svn repository
				   and then build zipped tar files for a distribution

  checkout-all.sh		-- script to checkout all of the sources from the
				   repository; use the `-e` option to specify the
				   **export** command to svn.

  cvs2svn-smlnj.options

  freeze-as.sh

  make-release.sh		-- tags sources with a release number

  refresh-all.sh		-- refresh all of the sources

  stat-all.sh			-- run `svn status` on all of the sources

  svn-upgrade-all.sh		-- run `svn upgrade` on all of the sources

  switch-all-svn-https.sh

  zip-bin-lib.sh
