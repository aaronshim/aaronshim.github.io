---
title: "Work"
description: "A summary of my professional work, talks, and publications."
lang: "en-us"
---

# Work

I've always been fascinated by the empowering nature of programming and its
tools. I love creating the right building blocks that help more people
experience the joy of building whatever they can imagine.

I currently work at Google in the Security Engineering organization, where my
focus is on web security (particularly on the frontend), the security of AI and
agentic systems, and establishing safe development practices for an organization
of hundreds of thousands of developers. My work centers on providing them with
the right building blocks to develop secure-by-default software, freeing up
their mindshare to focus on shipping amazing features to end users.

Our team has a history of shipping large-scale web security remediations, like
strict CSP and Trusted Types, which are designed to eliminate entire classes of
bugs by following Secure by Design principles. We've applied these solutions
across hundreds of web applications serving billions of users. Since then, much
of my work has focused on taking the lessons from these efforts and sharing them
with the wider open-source JavaScript community through conference talks,
technical writing, and participation in industry-wide consortiums.

Previously, I worked on product teams on Google Workspace and Google Cloud.
Prior to Google, I worked on Windows UI frameworks and their debugging tooling
inside Visual Studio for Microsoft's Developer Division.

## Talks

I'm passionate about sharing knowledge, especially in formats that allow for the
cross-pollination of ideas and interaction. I am incredibly grateful for all the
conferences and venues that have hosted my colleagues and me, facilitating the
spread of this knowledge to the wider community.

### Trusted Types

[Trusted Types](https://developer.mozilla.org/en-US/docs/Web/API/Trusted_Types_API)
is a [web platform feature](https://www.w3.org/TR/trusted-types/) meant to
provide comprehensive protection against
[DOM Cross-Site Scripting (XSS)](https://portswigger.net/web-security/cross-site-scripting/dom-based)
and work alongside
[Strict CSP](https://developer.mozilla.org/en-US/docs/Web/HTTP/Guides/CSP#strict_csp)
to shut down clientside [XSS](https://owasp.org/www-community/attacks/xss/) as
an entire class of vulnerabilities. In the following talks, we try to explain
the importance of XSS protections to a modern web developer, explain how Trusted
Types is a defense-in-depth mechanism, and share some lessons learned and best
practices from the many remediations across hundreds of webapps and billions of
developers.

- [LeHack 2023](https://www.youtube.com/watch?v=hOOpn0xEqrQ)
- [LibertyJS 2023](https://x.com/liberty_js/status/1712846893628191081)
- [Frontrunners DC 2024](https://www.youtube.com/watch?v=NUj7-XM51y8&t=12s)

### Safe Coding in JavaScript / TypeScript

The Safe Coding philosophy is an idea that API design should be easy for
develoeprs to write secure-by-default applications with and difficult to misuse.
The following talks focus on how this philosophy is implemented for the
JavaScript and TypeScript ecosystem inside Google, how it works alongside web
platform features such as
[Trusted Types](https://developer.mozilla.org/en-US/docs/Web/API/Trusted_Types_API)
and
[Strict CSP](https://developer.mozilla.org/en-US/docs/Web/HTTP/Guides/CSP#strict_csp)
to prevent
[Cross-Site Scripting (XSS)](https://owasp.org/www-community/attacks/xss/), and
our attempts at open-sourcing tooling that we found helpful in our experience.

- [AppSec Village @ DEFCON32 (2024)](https://www.youtube.com/watch?v=Q_PvndVqnsQ)
- BSides Seattle 2025
- [BSides Las Vegas 2025](https://www.youtube.com/live/DM2koEQ16p4?si=Abe3-qQ6jBGPYQO1&t=4322)
- [AppSec Village @ DEFCON33 (2025)](https://github.com/aaronshim/juice-shop) --
  an extended hands-on workshop with challenges derived from the material above.

## Publications

### Google Security Engineering Blog

It's important to me that the insights we've gained from remediating hundreds of
webapps contribute back to securing the wider JavaScript ecosystem. To that end,
we make an effort to write about externally-applicable techniques and best
practices.

- [Enabling Trusted Types in a Complex Web Application (Google Security Engineering Blog)](https://bughunters.google.com/blog/6037890662793216/enabling-trusted-types-in-a-complex-web-application-a-case-study-of-appsheet)
  -- A step-by-step recipe of how to enable Trusted Types -- a defense-in-depth
  mechanism against DOM XSS-- in a complex codebase
- [A Deep Dive into Trusted Types Violations (Google Security Engineering Blog)](https://bughunters.google.com/blog/5850786553528320/a-deep-dive-into-js-trusted-types-violations)
  -- A deeper dive into common patterns encountered while debugging Trusted
  Types violations across a large number of webapps.

### W3C Secure Web Application Guidelines Community Group

You can find our publications on the official
[repo](https://github.com/w3c-cg/swag/tree/main/docs). Please come join our
[plenary calls](https://www.w3.org/community/swag/)!
