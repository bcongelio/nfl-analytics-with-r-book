---
title: "An Introduction to NFL Analytics with R"
author: "Bradley J. Congelio"
date: "2022-06-16"
site: bookdown::bookdown_site
knit: bookdown::render_book
output: bookdown::bs4_book
documentclass: book
bibliography: [book.bib, packages.bib]
biblio-style: apalike
description: |
  This is a minimal example of using the bookdown package to write a book.
  The HTML output format for this example is bookdown::gitbook,
  set in the _output.yml file.
link-citations: yes
github-repo: rstudio/bookdown-demo
---



# Preface {.unnumbered}

This is a *sample* book written in **Markdown**. You can use anything that Pandoc's Markdown supports; for example, a math equation $a^2 + b^2 = c^2$.

## About The Book {.unnumbered}

Each **bookdown** chapter is an .Rmd file, and each .Rmd file can contain one (and only one) chapter. A chapter *must* start with a first-level heading: `# A good chapter`, and can contain one (and only one) first-level heading.

Use second-level and higher headings within chapters like: `## A short section` or `### An even shorter section`.

The `index.Rmd` file is required, and is also your first book chapter. It will be the homepage when you render the book.

## Who This Book is For {.unnumbered}

You can render the HTML version of this example book without changing anything:

1.  Find the **Build** pane in the RStudio IDE, and

2.  Click on **Build Book**, then select your output format, or select "All formats" if you'd like to use multiple formats from the same book source files.

Or build the book from the R console:


```r
bookdown::render_book()
```

To render this example to PDF as a `bookdown::pdf_book`, you'll need to install XeLaTeX. You are recommended to install TinyTeX (which includes XeLaTeX): <https://yihui.org/tinytex/>.

## About The Author {.unnumbered}

As you work, you may start a local server to live preview this HTML book. This preview will update as you edit the book when you save individual .Rmd files. You can start the server in a work session by using the RStudio add-in "Preview book", or from the R console:


```r
bookdown::serve_book()
```



## Technical Details {.unnumbered}

technical details are goig here.
