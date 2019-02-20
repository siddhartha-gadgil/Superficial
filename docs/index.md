---
# Feel free to add content and custom Front Matter to this file.
# To modify the layout, see https://jekyllrb.com/docs/themes/#overriding-theme-defaults
title: Superficial Algorithms
layout: home
---

The main purpose of this site is to quickly share progress, including for checking. For now this is mainly in the form of notebooks made using _Jupyter-lab_.

## Notes (using Jupyter)

{% for note in site.notes %}
* [{{note.title}}]({{ site.baseurl }}/{{note.url}}.html)
{% endfor %}