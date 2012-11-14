#!/bin/sh

dirs=`find /srv/svn/ -maxdepth 1 -type d | sort`
for d in $dirs; do \
  p=`basename $d`
  echo -n "Getting svn logs for project $p ..."
  svn log $@ file://$d > /srv/rf/logs/analysis/svnlogs/`basename $d`.xml
  echo " done";
done
