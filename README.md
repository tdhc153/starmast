# starmast

St Andrews resources in mathematics and statistics. Please read this and `_site/styleguide.pdf` before starting a guide.

## What do you need to download to run the code?

[RStudio](https://posit.co/download/rstudio-desktop/) is the best editor to use for this project. If you do not already have RStudio, you can find this on AppsAnywhere, or download from the internet. RStudio has both a source editor and a visual editor.

Once it is downloaded, you can pull the repository, and open the `starmast.Rproj` file. This will open the entire workspace in RStudio, where you can then open a `.qmd` file to edit.

## What language will I be using?

The documents that you will be working on are `.qmd` files, which use **Quarto** (based on markdown) to manage the outputs. For resources on how to write in Quarto, please see the following links.

[Quarto homepage](https://quarto.org/)

[Quarto guide](https://quarto.org/docs/guide/)

[Quarto reference](https://quarto.org/docs/reference/)

Mathematics in Quarto is rendered using LaTeX. For a guide on writing mathematics in TeX, the following resource might be of use:

[Overleaf guides on how to write mathematics in LaTeX](https://www.overleaf.com/learn/latex/Mathematical_expressions)

Inclusion of mathematical expressions is split into two categories: in-line mathematics and display mathematics. In Quarto, in-line mathematics is included in dollar signs `$...$` and display mathematics is included in double dollar signs `$$...$$`. In particular, there should be no spaces between the dollar signs and the start/end of your mathematics; otherwise Quarto will not build.

The best way to learn is to get stuck in! Save new copies of the example guide/questions/answers on Introduction to quadratic equations and edit the code accordingly.

## How do I view my changes?

There are (initially) two ways to view your changes. The first is to 'render website'; this will build all `.qmd` files into a `.html` that you can view, and outputs the index page. The other files will be in the `_site` folder; you can open the `.html` files in your browser.

To build the `.pdf` and `.docx` versions of your work, you will need to first **save your work**, then go to the Console (bottom of default RStudio layout) and type in `quarto::quarto_render()`. This will build **every** file in the directory and takes a while (about one minute for the initial commit!). All outputs are stored in the `_site` folder, which you can then view.

## What packages do you need to install?

You should be using Quarto 1.3 (the latest stable version); you can download this at the following link.

[Quarto 1.3 download](https://quarto.org/docs/get-started/)

Other than this, running `quarto::quarto_render()` for the first time should prompt installation for the packages that are required to build the files.

The only exception to this is `webexercises`; Quarto support has not yet reached the version of this package on CRAN. Instead, you will need to run the following command in the console:

    devtools::install_github("psyteachr/webexercises")

## Which files should be left alone?

You should generally be wary of any files that are not `.qmd`, `.md`, `.html`, `.docx`, `.pdf`, and you should only really be editing `.qmd` files to produce content.

There are a few files which will be heavily scrutinised upon any commit. These are listed below, with brief comments as to why they should not be meddled with lightly.

-   `_quarto.yml` this file contains all the output information that directs what happens when `quarto::quarto_render()` is run. This file might need to be edited for website structure and output tweaking. Please make sure to let admins know about any changes to this file, and have an explanation for each change ready.

-   `starmast-doc-formatting.docx` and `starmastarticle.cls` are essentially 'template' files that govern the way `.docx` and `.pdf` files are outputted. Please do not edit these; instead, file any bugs or request any changes on the 'Issues' page.

-   `starmast.Rproj` is the workspace image management file for RStudio. Any commit of this file will have your workspace image on.

-   `styles.css` is a file that contains styles for the website. This is empty, and should be edited with admin permission only.

-   Any files in the `webexfiles` directory should be left well alone.

## Templates?

Feel free to create a copy of `introtoquadratics.qmd` to use as a template for your study guides; similarly, use `qs-` and `as-` for questions and answers. I am going to be quite strict with style, so I would thoroughly recommend you use these as templates.

## Any questions?

Raise as an Issue or post in Discussions. :)

## Version control

- v1.0 (initial readme commit, 18/05)
