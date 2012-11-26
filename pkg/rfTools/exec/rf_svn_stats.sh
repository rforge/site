#!/bin/sh

LOG_DIR=/srv/rf/logs/analysis/svnlogs
dirs=`find /srv/svn/ -mindepth 1 -maxdepth 1 -type d | sort`
for d in $dirs; do \
  p=`basename $d`
  echo -n "Getting svn logs for project $p ..."
  svn log --verbose --xml $@ file://$d > $LOG_DIR/`basename $d`.xml
  echo " done";
done

R --vanilla <<EOF
logfiles <- Sys.glob("$LOG_DIR/*.xml")

logs <- lapply(logfiles, function(log) {
    message(sprintf("Processing %s", log))
    tryCatch(rfTools:::read_svn_verbose_xml_log(log), error = identity)
})
names(logs) <- tools::file_path_sans_ext(basename(logfiles))

ind <- sapply(logs, inherits, "error")

names(logs)[ind]

ind <- !ind & sapply(logs, NROW)

logs <- do.call(rbind,
                Map(cbind, project = names(logs)[ind], logs[ind]))

saveRDS(logs, file = "$LOG_DIR/svnlogs.rds")

EOF
