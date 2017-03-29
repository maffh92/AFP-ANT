---
author: Martijn Fleuren
        Marinus Oosters
        Carlos Tomé Cortiñas
        Matthew Swart
title: Ants
subtitle: Amazing ants!
theme: uucs
mainfont: Ubuntu Light
sansfont: Ubuntu Light
---

Please use Markdown to write your slides. 

This makes sure that slides will be consistent -- and easy for me to
edit in the future.

---

Start a new slide with by beginning a new line three dashes `---`.

For example:

```
---

My contents

---
```

---

# Overview

* Architecture
* are pretty easy
* too!

---

# Architecture

slease include any images in the `img` subdirectory.

You can refer to images using the usual markdown syntax:

![My caption](img/uueduc.jpg "Alt caption"){ width=30% }

---

# DSL

```haskell
newtype AntT m l a = AntT { runAnt :: TardisT (Program l) l m a
```

---

# How does it works?

---

# Abstractions over AntT

---

# Making slides

I've included a Makefile to build slides.

You will need to have the Haskell tool `pandoc` installed:

```
> cabal install pandoc
> make
```
---

# Working with markdown

You may want to install the markdown mode for emacs (or some other
editor of choice).

I've included some file local variables at the bottom of this file --
you may find them useful.

---

# Inline latex

You can always use \emph{inline} \LaTeX commands if you want.

But try to avoid this if you can.

Most Markdown commands should suffice.

\LaTeX is useful for formula's

\begin{equation}
\tau + x = \sigma
\end{equation}

Or inline formulas, enclosed in dollar symbols like so $\tau + x$.

---

# Questions

If you can't get things to work, don't hesitate to get in touch!


<!-- Local Variables:  -->
<!-- pandoc/write: beamer -->
<!-- pandoc/latex-engine: "xelatex" -->
<!-- pandoc/template: "beamer-template.tex" -->
<!-- End:  -->
