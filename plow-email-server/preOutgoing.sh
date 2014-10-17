cp   ./dist/build/plowemail/plowemail ./
tar -czvf plowemail.tar.gz plowemail
rsync -avzhe ssh --progress plowemail.tar.gz node@108.168.240.123:/home/node/incoming/
rm ./plowemail
rm ./plowemail.tar.gz
