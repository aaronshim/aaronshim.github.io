---
layout: page
title: Archive
---

Here are some past things I've written:

{% for post in site.posts %}
{{ post.date | date:"%B" }}: <a href="{{ post.url }}">{{ post.title }}</a>
{% endfor %}