#!/bin/bash
set -e
rsync -av math.iisc.ac.in:~/public_html/catg2020/bin/ CATG2020/static/bin/
cp CATG2020/notebooks/bin/superficial-*.jar CATG2020/static/bin || true
cd CATG2020
hugo
rsync -avz public/ math.iisc.ac.in:/home/gadgil/public_html/catg2020/
