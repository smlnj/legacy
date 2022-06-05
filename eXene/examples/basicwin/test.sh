#!/bin/ksh
#

ROOT=/home/sml/Dev/jhr/109.x

$ROOT/src/runtime/objs/run.mipseb-irix5 @SMLload=$ROOT/bin/.heap/sml-cm <<XXXX
  CM.make();
  BasicWin.doit'(["/"], ":0.0", 20);
XXXX

