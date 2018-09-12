# Title: Shell script to monitor tweet collection engine
# Client: Prof. Dr. Pedro Gras-Manzano/Prof. Dr. Frank Brisard (Univerity of Antwerp)
# Description: This script can be run with a cron tab every minute. It requests the list of running processes and filters these for the name of the tweet collection script (tweets.R). If there is no line for this R script, it will restart the script. Otherwise, it will do nothing. 
# Author: Jeroen Claes
# Date:  Sun Dec 24 13:34:13 2017


PROCESS=$(ps -ef | grep "projectQue.R" | grep -v "grep" | wc -l)

if [ $PROCESS -gt 0 ]; then
echo "projectQue.R is working" > /mnt/disks/storage/projectQue/shellLogTweets.txt
else
echo "projectQue.R stopped, restarting" >shellLogTweets.txt
Rscript /mnt/disks/storage/projectQue/code/projectQue.R > /mnt/disks/storage/projectQue/tweets.log 2>&1&
fi