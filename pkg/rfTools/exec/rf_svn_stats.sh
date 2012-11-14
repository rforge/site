#!/bin/sh

dirs=`find /srv/svn/ -maxdepth 1 -type d | sort`
for d in $dirs; do \
  p=`basename $d`
  echo -n "Getting svn logs for project $p ..."
  svn log $@ file://$d > ~/tmp/svnlogs/`basename $d`.log
  echo " done";
done
