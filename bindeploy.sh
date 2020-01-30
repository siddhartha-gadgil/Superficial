#!/bin/bash
set -e
rsync -av --progress math.iisc.ac.in:~/public_html/catg2020/bin/ CATG2020/notebooks/bin/ || true
rsync -av CATG2020/notebooks/bin/superficial-*.jar CATG2020/static/bin/ || true
./mill superficial.docs
cd CATG2020
chmod a+r static/bin/*
./mknotes.sh
hugo
rsync -avz --progress public/ math.iisc.ac.in:/home/gadgil/public_html/catg2020/
cd ..
git add .
git commit -m "commit on deploy; rebuilt scaladocs, notebooks"
git push
