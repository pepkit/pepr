wget https://github.com/pepkit/example_peps/archive/master.zip
unzip master.zip
rm -rf inst/extdata/example_peps-master 
mv example_peps-master inst/extdata
rm master.zip