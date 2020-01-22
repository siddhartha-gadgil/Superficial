#!/bin/bash
set -e
rsync -av --progress math.iisc.ac.in:~/public_html/catg2020/bin/ CATG2020/static/bin/
rsync -av CATG2020/static/bin/ CATG2020/notebooks/bin/
rsync -av CATG2020/notebooks/bin/superficial-*.jar CATG2020/static/bin/ || true
./mill superficial.docs
cd CATG2020
./mknotes.sh
hugo
rsync -avz --progress public/ math.iisc.ac.in:/home/gadgil/public_html/catg2020/
