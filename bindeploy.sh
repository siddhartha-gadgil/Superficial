#!/bin/bash
set -e
cp superficial/notebooks/superficial-*.jar CATG2020/static/bin
cd CATG2020
hugo
rsync -avz public/ math.iisc.ac.in:/home/gadgil/public_html/catg2020/
