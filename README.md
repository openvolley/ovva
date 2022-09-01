
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ovva

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
![openvolley](https://img.shields.io/badge/openvolley-darkblue.svg?logo=data:image/svg%2bxml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIyMTAiIGhlaWdodD0iMjEwIj48cGF0aCBkPSJNOTcuODMzIDE4Ny45OTdjLTQuNTUtLjM5Ni0xMi44MTItMS44ODYtMTMuNTgxLTIuNDQ5LS4yNDItLjE3Ny0xLjY5Mi0uNzUzLTMuMjIyLTEuMjgxLTI4LjY5Ni05Ljg5NS0zNS4xNy00NS45ODctMTMuODY4LTc3LjMyMyAyLjY3Mi0zLjkzIDIuNTc5LTQuMTktMS4zOTQtMy45MDYtMTIuNjQxLjktMjcuMiA2Ljk1Mi0zMy4wNjYgMTMuNzQ1LTUuOTg0IDYuOTI3LTcuMzI3IDE0LjUwNy00LjA1MiAyMi44NjIuNzE2IDEuODI2LS45MTgtLjE3LTEuODktMi4zMS03LjM1Mi0xNi4xNzQtOS4xODEtMzguNTYtNC4zMzctNTMuMDc0LjY5MS0yLjA3IDEuNDE1LTMuODY2IDEuNjEtMy45ODkuMTk0LS4xMjMuNzgyLTEuMDUzIDEuMzA3LTIuMDY2IDMuOTQ1LTcuNjE3IDkuNDU4LTEyLjg2MiAxNy44MzktMTYuOTcgMTIuMTcyLTUuOTY4IDI1LjU3NS01LjgyNCA0MS40My40NDUgNi4zMSAyLjQ5NSA4LjgwMiAzLjgwMSAxNi4wNDcgOC40MTMgNC4zNCAyLjc2MiA0LjIxMiAyLjg3NCAzLjU5NC0zLjE3My0yLjgyNi0yNy42ODEtMTYuOTA3LTQyLjE4NS0zNi4wNjgtMzcuMTUxLTQuMjU0IDEuMTE3IDUuMjQtMy4zMzggMTEuNjYtNS40NzMgMTMuMTgtNC4zOCAzOC45MzctNS43NzIgNDYuMDc0LTEuNDg4IDEuMjQ3LjU0NyAyLjIyOCAxLjA5NSAzLjI3NSAxLjYzIDQuMjkgMi4xMDcgMTEuNzMzIDcuNjk4IDE0LjI2NSAxMS40MjcuNDA3LjYgMS4yNyAxLjg2NiAxLjkxNyAyLjgxNCAxMS4zMDggMTYuNTY1IDguNjIzIDQxLjkxLTYuODM4IDY0LjU1Mi0zLjI0OSA0Ljc1OC0zLjI1OCA0Ljc0MiAyLjQ1IDQuMDE4IDMyLjQ4Mi00LjEyMiA0OC41MTUtMjEuOTM1IDM5LjU3OC00My45NzQtMS4xNC0yLjgwOSAxLjU2NiAxLjA2IDMuNTE4IDUuMDMyIDI5LjY5MyA2MC40MTctMjIuNTggMTA3Ljg1My03OS40OTggNzIuMTQzLTUuMDg0LTMuMTktNS4xMjMtMy4xNTItMy45MDIgMy44ODMgNC43MjEgMjcuMjIgMjUuNzgzIDQzLjU2MiA0NC4wODkgMzQuMjEgMS4zNjItLjY5NiAyLjIxLS43NSAyLjIxLS4xNDMtNi43NiAzLjg1Ny0xNi4wMTggNi41NTMtMjMuMTI2IDguMDkxLTcuNTU1IDEuNTQ3LTE4LjM2NiAyLjE3Mi0yNi4wMiAxLjUwNnoiIGZpbGw9IiNmZmYiLz48ZWxsaXBzZSBjeD0iMTA1Ljk3NSIgY3k9IjEwNC40NDEiIHJ4PSI5NC44NCIgcnk9IjkyLjU0MiIgZmlsbD0ibm9uZSIgc3Ryb2tlPSIjZmZmIiBzdHJva2Utd2lkdGg9IjEwLjc0Ii8+PC9zdmc+)
[![R build
status](https://github.com/openvolley/ovva/workflows/R-CMD-check/badge.svg)](https://github.com/openvolley/ovva/actions)
[![Codecov test
coverage](https://codecov.io/gh/openvolley/ovva/branch/master/graph/badge.svg)](https://codecov.io/gh/openvolley/ovva?branch=master)
<!-- badges: end -->

## About

This R package provides a Shiny app for viewing volleyball match videos
in conjunction with scout files.

## Live demo

Try it for yourself: <https://openvolley.shinyapps.io/ovva_demo/>

(This demo is running on a free (time-limited) shinyapps tier, so it may
not be available if too many people have used it recently.)

## Installation

``` r
options(repos = c(openvolley = "https://openvolley.r-universe.dev",
                  CRAN = "https://cloud.r-project.org"))

install.packages("ovva")

## or

## install.packages("remotes") ## if needed
remotes::install_github("openvolley/ovva")
```

## Usage

Try the inbuilt demo:

``` r
library(ovva)
ovva_shiny_demo()
```

Or start the shiny app with the path to your DataVolley files:

``` r
ovva_shiny(data_path = c(PL2018 = "data/volley/PlusLiga-2018_19"))
```

------------------------------------------------------------------------

![](man/figures/ovva.gif)

------------------------------------------------------------------------

### Notes

1.  In order for the app to find the video file associated with each dvw
    file, the video file path must be present in the dvw file. You can
    check this by inspecting the dvw file in a text editor — you should
    see something like:

<!-- -->

    [3RESERVE]
    [3VIDEO]
    Camera0=D:\video\2019_03_01-KATS-BEDS.mp4
    [3SCOUT]

2.  If your videos are hosted online, you don’t need them locally. This
    includes YouTube videos (they must be either ‘public’ or ‘unlisted’,
    but not ‘private’). To use an online video, enter the URL as the
    video location:

<!-- -->

    [3RESERVE]
    [3VIDEO]
    Camera0=https://www.youtube.com/watch?v=NisDpPFPQwU
    [3SCOUT]

and start `ovva` with `video_server = "none"`:

``` r
ovva_shiny(data_path = c(MyData = "my/dvw/path"), video_server = "none")
```

(note that with remote video files you still need your dvw files locally
— this is the `data_path` folder above).

3.  The video server runs in a separate thread to the shiny application
    itself. If the `lighttpd` executable is present on the system path,
    this will be used for serving videos by default, otherwise the
    `servr` R package will be used. The performance of `lighttpd` is
    better, so you might want to install this if you don’t already have
    it. On Windows, you can use `ovva_install_lighttpd()` to do this. On
    other platforms, you’ll have to do it yourself. See
    <https://www.lighttpd.net/download/>.

------------------------------------------------------------------------

Interested in this but you aren’t an R user, or you want to be able to
easily share this functionality with your players or coaches? See [this
hosted version](https://apps.untan.gl/ovva/) (available by
subscription).
