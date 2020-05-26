#!/bin/bash

if [ $# -ne 1 ]; then
    echo $0: usage: update_test_data.sh branch
    exit 1
fi

branch=$1

wget https://github.com/pepkit/example_peps/archive/${branch}.zip
unzip ${branch}.zip
rm -rf inst/extdata/example_peps-${branch} 
mv example_peps-${branch} inst/extdata
rm ${branch}.zip