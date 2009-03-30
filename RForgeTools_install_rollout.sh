#!/bin/bash
## RForgeTools, infrastructure package roll out
## File: RForgeTools_install_rollout.sh
## Theussl, 2009

## build hosts username rforge
BUILD_MACHINES="xmorthanc.wu.ac.at r-apple.wu.ac.at"
## build host username theussl
BUILD_MACHINES_TH="gimli.wu.ac.at"

## build package
R CMD build ./pkg
RFORGETOOLS=`ls RForgeTools*.tar.gz`

for machine in ${BUILD_MACHINES}; do    
    scp -B ${RFORGETOOLS} rforge@${machine}:/home/rforge/${RFORGETOOLS}
    ssh rforge@${machine} 'R CMD INSTALL ~/RForgeTools*.tar.gz ; \
rm -f ~/RForgeTools*.tar.gz'
done

for machine in ${BUILD_MACHINES_TH}; do
    scp ./${RFORGETOOLS} ${machine}:~/${RFORGETOOLS}
    ssh ${machine} 'R CMD INSTALL ~/RForgeTools*.tar.gz ; \
rm -f ~/RForgeTools*.tar.gz'
done

rm -f RForgeTools*.tar.gz

exit 0