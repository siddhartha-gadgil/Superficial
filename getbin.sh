#!/bin/bash
set -e
rsync -av math.iisc.ac.in:~/public_html/catg2020/bin/ CATG2020/static/bin/
rsync -av CATG2020/static/bin/ CATG2020/notebooks/bin/
