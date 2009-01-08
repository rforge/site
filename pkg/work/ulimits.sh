#!/bin/bash

echo "Before setting the limits we have:"
echo ""
ulimit -a

## core size (size of a core dump of a program). default 0
ulimit -c 0
## main memory limit, default unlimited -> we set it to 256MB
ulimit -m 262144
## virtual memory (swap space), default unlimited -> we set it to 256MB
ulimit -v 262144
## main memory limit, default unlimited -> we set it to 256MB
ulimit -l 262144 
## max user processes, default 266 (Linux unlimited) -> we set it to 200
ulimit -u 200
## max cpu time, default unlimited -> we set it to 600s
ulimit -t 600
## max file size, default unlimited -> we set it to 256MB
ulimit -f 524288


echo ""
echo "now we have:"
echo ""
ulimit -a
echo ""
echo "done"

R --vanilla <<EOF
writeLines("Starting with a vector which fits in memory")
n <- 5000
system.time(vec <- rnorm(n*n))
object.size(vec)/(1024*1024)
save(vec, file = "test190.sav")
writeLines("Exceeds limit:")
n <- 10000
system.time(vec <- rnorm(n*n))
object.size(vec)/(1024*1024)
save(vec, file = "test790.sav")

EOF

echo "The shell has not yet quit"
echo "Quitting now"
exit 0
