# MWRA Biobot COVID Wastewater Monitor

## tl;dr

See and download metro Boston Covid wastewater testing data as a [CSV](https://github.com/smach/biobot_mwra/blob/main/data/processed/combined_data.csv) and not just a PDF. Explore the data in an [interactive dashboard](https://smach.github.io/biobot_mwra/) that lets you set date ranges and view error bars.

## Longer Overview

This project, written partly by Claude Opus 4.5, streamlines and improves some code I wrote pre-GenAI years ago to monitor [Massachusetts Water Resources Authority (MWRA) Biobot Covid wastewater testing data](https://www.mwra.com/biobot/biobotdata.htm). I never made that code public because, well, I wasn't super proud of it 😅 -- I wrote it in a hurry at the start of the pandemic and never really rationalized it or cleaned it up.

That code still works, including sending me an email whenever the data updates! You can see my R Shiny app (but not the code) at [https://apps.machlis.com/shiny/ma_corona_virus/](https://apps.machlis.com/shiny/ma_corona_virus/). Most of the tabs haven't updated for several years, but the main opening screen should still be updating.

But back to this repo. I decided to ask Claude Opus 4.5 to write some basic code to track and visualize the MWRA covid testing data. I still had to steer it to create the visualizations I wanted (the error bars took a surprising amount of back and forth until I just uploaded the code I'd written a few years ago, for example. It was satisfying to still be able to do some coding better than generative AI!).

And, for the first time, I tried Posit's [Claude Skill for testing R packages](https://github.com/posit-dev/skills/tree/main/r-lib/testing-r-packages), which wrote _all_ the tests for this project!

IMPORTANT: I did not rigorously review the test code to make sure it tests exactly what I'd want to test. Use this repository at your own risk!

Below is an explanation of code in the repo, written mostly by Claude and lightly edited by me:

## View the Data

### CSV Files

The processed data is available in CSV format in the `data/processed/` folder:

- **combined_data.csv** - All data from both wastewater systems
- **north_system.csv** - North system data only
- **south_system.csv** - South system data only

Each CSV contains these columns:
| Column | Description |
|--------|-------------|
| `date` | Sample date |
| `copies_per_ml` | COVID viral RNA copies per milliliter |
| `seven_day_avg` | 7-day rolling average |
| `lower_ci` | Lower 95% confidence interval |
| `upper_ci` | Upper 95% confidence interval |
| `system` | "North" or "South" (combined_data.csv only) |

### Source PDF

The original data from MWRA is stored at `data/latest_data.pdf`. This PDF contains the most recent raw tables and charts published by MWRA/Biobot.

## Interactive Dashboard

**[View the Interactive Dashboard](https://smach.github.io/biobot_mwra)** 

Features:
- Toggle between North and South wastewater systems
- Daily values or 7-day averages
- Adjustable date range (90 days, 6 months, 1 year, all data)
- Optional 95% confidence intervals

Note that if you want to look at the interactive dashboard file locally on your own system, it's at `docs/index.html` _but it needs a Web server to function,_ you can't just open the index.html file in a browser. If you have the R servr package installed (you can install it with `install.packages("servr")` ) you can run 

`servr::httd("docs")`

## Requirements

Want to download and run this yourself? You'll need these R packages:

```r
install.packages(c(
  "rvest",
  "httr2",
  "pdftools",
  "dplyr",
  "readr",
  "ggplot2",
  "scales",
  "jsonlite"
))
```

## Usage

Run from the project directory:

```r
source("run_monitor.R")
```

To force an update even when there's no new data, edit `run_monitor.R` and set `force <- TRUE` at the top.

The script will:
1. Check MWRA website for new data
2. Download PDF if new data available
3. Extract data to CSV files
4. Generate plots
5. Update the web dashboard

## Data Source

[MWRA Biobot Data Page](https://www.mwra.com/biobot/biobotdata.htm)

## Output Files

Only the most recent PDF and CSV files are kept (overwritten on each update).

**Source PDF**
- `data/latest_data.pdf` - Most recent PDF from MWRA

**Data (CSV)**
- `data/processed/combined_data.csv` - All data, both systems
- `data/processed/north_system.csv` - North system only
- `data/processed/south_system.csv` - South system only

**Plots (PNG)**
- `output/plots/north_90days.png`
- `output/plots/south_90days.png`

## GitHub Actions

The workflow runs on a cron job and:
1. Checks for new data
2. Downloads and processes new PDFs
3. Commits updated data and plots
4. Deploys dashboard to GitHub Pages
5. Creates a GitHub Issue notification

### Container-based runtime

To keep runs reproducible, `check-data.yml` runs inside a prebuilt container
image rather than installing R packages fresh on every run (fresh installs
occasionally produced an `rlang.so: undefined symbol: SETLENGTH` crash). The
image is defined by the `Dockerfile` (pinned R + packages from a dated Posit
Package Manager snapshot, with `curl-impersonate` baked in) and is built and
pushed to the GitHub Container Registry by `.github/workflows/build-image.yml`,
which only runs when the `Dockerfile` changes.

> **After editing the Dockerfile:** a push that changes both the `Dockerfile`
> and pipeline files starts the data check and the image build at the same
> time, so the data check can fail to pull the not-yet-built image ("manifest
> unknown"). That red X can be ignored: `check-data.yml` re-runs automatically
> as soon as the image build completes successfully.

### Handling MWRA's bot wall

The MWRA site sits behind an Imperva bot challenge that intermittently serves a
"please wait while we verify your request" page instead of the data.
`curl-impersonate` clears it most of the time; when it doesn't, the pipeline
treats it as a **transient no-op** and exits cleanly (green) so it doesn't raise
a false alarm — the next run almost always recovers. As a safety net, if the
newest processed data is more than 14 days old *and* a challenge is still
happening, the run fails loudly instead (either the bypass has genuinely broken
or MWRA has stopped publishing — both deserve a look).

### Setup

1. Push to GitHub
2. Go to **Settings** > **Pages**
3. Set source to **GitHub Actions**
4. Dashboard will be at `https://YOUR_USERNAME.github.io/biobot_mwra`

## Project Structure

```
biobot_mwra/
├── .github/workflows/
│   ├── check-data.yml
│   └── build-image.yml
├── Dockerfile
├── R/
│   ├── utils.R
│   ├── 01_check_updates.R
│   ├── 02_download_pdf.R
│   ├── 03_extract_data.R
│   └── 04_visualize.R
├── data/
│   ├── latest_data.pdf
│   └── processed/
├── docs/
│   ├── index.html
│   └── data/
├── output/plots/
├── state/
│   └── last_update.json
├── run_monitor.R
└── README.md
```
