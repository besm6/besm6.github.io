---
layout: page
title: Новости
permalink: /blog/
topmenu: yes
---

{% for post in site.posts %}
# [{{ post.title }} &mdash; {{ post.date | date_to_string }}]({{ post.url }})
  {{ post.content }}
{% endfor %}
