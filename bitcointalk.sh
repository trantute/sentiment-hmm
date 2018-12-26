#!/bin/bash
#rm output; # complains as long as there is no output file
for i in {3467..4058} # the set of pages
do
   # seems to be necessary, otherwise bitcointalk.org blocks,
   # thus download takes some time, without it would be faster
   sleep 3;

   # compute the page index
   index=$(((i-1) * 20));

   # download of the whole "Der Aktuelle Kursverlauf"-Thread
   # since a page might block, if a bot is downloading, lets mask the bot
   # can be extended with addition firefox versions

   # sample an integer from {0,1}
   r=$(shuf -i 0-1 -n 1);

   # if r equals 0, then fake a firefox for linux
   if [[ $r == "0" ]]
   then
      ua="Mozilla/5.0 (X11; U; Linux i686; de; rv:1.9b5) Gecko/2008050509 Firefox/3.0b5";
   fi

   # if r equals 1, then fake a firefox for OSX
   if [[ $r == "1" ]]
   then
      ua="Mozilla/5.0 (Macintosh; Intel Mac OS X 10.8; rv:21.0) Gecko/20100101 Firefox/21.0";
   fi

   # get the data
   wget --user-agent="$ua" -O - "https://bitcointalk.org/index.php?topic=26136.$index" 1>>output 2>/dev/null;

   echo $i;
done
# grep the dates and save the results in dates.txt
# bitcointalk-html might change in future so that grep command must be adapted
grep -Eo "<div class=\"smalltext\">[JFMAJSOND][a-z]+[[:space:]][0-9]{2}[,][[:space:]][0-9]{4}[,][[:space:]][0-9]{2}[:][0-9]{2}[:][0-9]{2}[[:space:]][PA][M]" output | grep -Eo "[JFMAJSOND][a-z]+[[:space:]][0-9]{2}[,][[:space:]][0-9]{4}[,][[:space:]][0-9]{2}[:][0-9]{2}[:][0-9]{2}[[:space:]][PA][M]" > dates.txt
