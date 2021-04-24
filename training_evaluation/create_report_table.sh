#! /bin/bash

#
# process a directory that contains individual log files with tree size estimation reports by SCIP
#
# ensure that, e.g., different settings are not mixed within one directory
#
# the training only happens on instances that have been reported solved
#
# create a file that contains all problems for which SCIP finished

echo "Usage: ${0} <logfile-directory> [outputdir]"
if [ -z ${1} ]
then
   echo "Exiting because missing logfile directory (mandatory)"
   exit 1
else
   logfiledirectory=${1}
fi

if [ -z ${2} ]
then
   outputdir="output"
else
   outputdir=${2}
fi
echo "Storing all relevant information in directory ${outputdir}/"
mkdir -p ${outputdir}
 grep -l "problem is solved" ${logfiledirectory}/*.out > ${outputdir}/solvedproblems.list
 echo "Number of solved problems: $(cat ${outputdir}/solvedproblems.list | wc -l), stored in ${outputdir}/solvedproblems.list"

 for i in `cat ${outputdir}/solvedproblems.list`
 do
    awk -f $(dirname ${0})/parse_report.awk $i
 done | sed -e "s/progress/tree-weight/g" > ${outputdir}/table1.csv

 for i in `cat ${outputdir}/solvedproblems.list`
 do
    actual=$(grep "Tree Data\|Estimation Tree" $i |\
                tail -n 1 | \
                grep -oP "(\d+) nodes" | \
                sed 's/ nodes//g')
    time=$(grep -i "Total Time" $i | \
             grep -oP '[^ ]+$' \
            )
    echo "$i,$actual,$time"
done > ${outputdir}/actual.csv

./process_data.R ${outputdir}