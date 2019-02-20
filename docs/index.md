---
# Feel free to add content and custom Front Matter to this file.
# To modify the layout, see https://jekyllrb.com/docs/themes/#overriding-theme-defaults

layout: home
---

## Notes (using Jupyter)

{% for note in site.notes %}
* [{{note.title}}]({{ site.baseurl }}/{{note.url}}.html)
{% endfor %}