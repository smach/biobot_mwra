# MWRA Biobot COVID Wastewater Monitor

## tl;dr

See and download metro Boston Covid wastewater testing data as a [CSV](https://github.com/smach/biobot_mwra/blob/main/data/processed/combined_data.csv) and not just a PDF. Explore the data in an [interactive dashboard](https://smach.github.io/biobot_mwra/) that lets you set date ranges and view error bars.

There are now **two independent pipelines** in this repo for the same metro Boston sewershed:

| | MWRA Biobot | WastewaterSCAN |
|---|---|---|
| Source | [MWRA Biobot PDF](https://www.mwra.com/biobot/biobotdata.htm) | [WastewaterSCAN](https://data.wastewaterscan.org/) JSON feed |
| Frequency | Roughly weekly, and it has paused for weeks at a time | Two to three samples a week |
| Units | Copies per mL of wastewater, flow-adjusted | Gene copies per gram dry solids, PMMoV-normalized |
| Runner | `run_monitor.R` | `run_wwscan.R` |

The two measure the same virus in the same sewershed with different lab methods, so **their numbers cannot be compared or combined**. They're kept as separate series everywhere: separate CSVs, separate state files, separate charts.

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

### WastewaterSCAN CSV

`data/processed/wwscan_covid.csv` holds the SARS-CoV-2 series for the Deer Island
treatment plant (the "Boston, MA" site on the WastewaterSCAN dashboard, serving
about 2.4 million people).

| Column | Description |
|--------|-------------|
| `date` | Sample collection date |
| `covid` | Smoothed (5-sample trimmed mean), PMMoV-normalized, × 1,000,000 — the line WastewaterSCAN plots |
| `covid_unsmoothed` | The single-sample value, same units |
| `lower_ci` / `upper_ci` | 95% interval around `covid_unsmoothed` |
| `level` | `low` / `medium` / `high` against national tertile cutoffs |
| `activity_category` | WastewaterSCAN's own per-sample label, which is *plant-relative* and will often disagree with `level` |

`docs/data/wwscan_summary.json` carries the current headline, level, trend, and
change sentence. It exists so the dashboard and the email script display the
same numbers instead of each recomputing them.

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

## The WastewaterSCAN pipeline

MWRA sends its Deer Island samples to WastewaterSCAN as well as to Biobot.
When Biobot went quiet for weeks in July 2026, that second feed was still
publishing every two or three days — hence this pipeline.

```r
source("run_wwscan.R")
```

It fetches the feed, extracts the SARS-CoV-2 series, writes the CSV and summary,
and updates `state/wwscan_state.json`. `.github/workflows/check-wwscan.yml` runs
it daily and files a GitHub issue whenever there's a new sample.

### Where the data actually comes from

`data.wastewaterscan.org` is a static site that reads per-plant JSON straight
from a public Google Cloud Storage bucket. This pipeline reads the same file:

```
https://storage.googleapis.com/wastewater-dev-data/json/b50c6424-02d1-482f-b928-dbed1d7eab25.json
```

That is an internal endpoint, not a documented API, and the bucket is named
`wastewater-dev-data` — it could move without notice. **If it ever 404s or stops
returning JSON, the run fails loudly** rather than quietly going stale. To
re-derive the URL: load the dashboard, look through its `/_nuxt/*.js` bundles for
`storage.googleapis.com`, and find the `plants.json` / per-plant URL builders.
`plants.json` also lets you look the Deer Island plant ID up again by name.

### How the level and trend are calculated

Both deliberately mirror what the WastewaterSCAN dashboard itself reports, so
an email and their site never appear to contradict each other:

- **Level** compares the latest smoothed value against national 33rd/66th
  percentile cutoffs for the N Gene target (20.33 and 105.40 in these units).
  Those cutoffs are hardcoded in the dashboard's JavaScript rather than served
  as data, so they're copied into `R/05_wwscan.R` as constants. Re-check them
  the same way you'd re-derive the URL above.
- **Trend** is an ordinary least squares fit of the *unsmoothed* values over the
  trailing 21 days, called a trend only when p < 0.05. The unsmoothed series
  matters here: the smoothed one is autocorrelated by construction and would
  report a significant trend nearly every week.

Verified against the dashboard on 2026-08-08, when it reported "SARS-CoV-2
Medium / No trend in the last 21 days and medium concentration" — which is
exactly what this code produces for the same data.

### Attribution and terms

Data comes from [WastewaterSCAN](https://www.wastewaterscan.org/), a program run
by Stanford and Emory with Verily. They ask that any use of the data cite them,
which the dashboard, the GitHub issues, and the emails all do.

One thing worth knowing: the Boston plant record carries `allow_download: false`,
which is what hides the dashboard's own CSV download button for this site. The
JSON this pipeline reads is public and unauthenticated — it's what the dashboard
itself loads to draw its charts — but that flag suggests the data partner didn't
opt into bulk downloads. If you're doing anything beyond personal/civic
monitoring, email them first: `info@wastewaterscan.org`.

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

`check-data.yml` (MWRA Biobot, twice daily) and `check-wwscan.yml`
(WastewaterSCAN, daily) both:

1. Check for new data
2. Process it
3. Commit updated files
4. Deploy the dashboard to GitHub Pages
5. Create a GitHub Issue notification

Both deploy jobs share a `pages` concurrency group so they can't deploy at the
same time, and the WastewaterSCAN job rebases before pushing since both commit
to `main`.

### When MWRA stops publishing

A long publishing pause used to be invisible: the page loads, nothing is new,
the run exits green, and nobody hears about it. That's exactly what happened in
July 2026. Now, when MWRA's newest published sample passes 14 days old,
`check-data.yml` opens a single `stale-data` issue and won't file another while
it stays open. The scraper isn't broken in that situation, so the run itself
still succeeds.

### Container-based runtime

To keep runs reproducible, `check-data.yml` runs inside a prebuilt container
image rather than installing R packages fresh on every run (fresh installs
occasionally produced an `rlang.so: undefined symbol: SETLENGTH` crash). The
image is defined by the `Dockerfile` (pinned R + packages from a dated Posit
Package Manager snapshot, with `curl-impersonate` baked in) and is built and
pushed to the GitHub Container Registry by `.github/workflows/build-image.yml`,
which only runs when the `Dockerfile` changes.

> **First-time / after editing the Dockerfile:** the image must exist before
> `check-data.yml` can run. On a merge that changes the `Dockerfile`, the
> scheduled check may fire before the image finishes building and fail to pull
> it — this self-heals on the next run. To avoid the blip, run the **Build
> container image** workflow (Actions → *Run workflow*) first.

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
│   ├── check-data.yml       # MWRA Biobot, twice daily
│   ├── check-wwscan.yml     # WastewaterSCAN, daily
│   └── build-image.yml
├── Dockerfile
├── R/
│   ├── utils.R              # state, retries, the single network entry point
│   ├── 01_check_updates.R
│   ├── 02_download_pdf.R
│   ├── 03_extract_data.R
│   ├── 04_visualize.R
│   └── 05_wwscan.R          # WastewaterSCAN fetch, parse, level, trend
├── data/
│   ├── latest_data.pdf
│   └── processed/           # *.csv for both pipelines
├── docs/
│   ├── index.html
│   └── data/                # combined_data.csv, wwscan_covid.csv, wwscan_summary.json
├── output/plots/
├── state/
│   ├── last_update.json     # MWRA
│   └── wwscan_state.json    # WastewaterSCAN
├── run_monitor.R            # MWRA pipeline
├── run_wwscan.R             # WastewaterSCAN pipeline
└── README.md
```

## Email notifications

I send myself an email when there's new data. That runs outside this repo and
just reads the published files below, so nothing here needs to know about it.

If you're reusing this code, treat these two as a **stable interface** rather
than internal dashboard files — anything consuming them over HTTP will break
silently if their fields are renamed, and this repo's tests won't catch it:

- `docs/data/wwscan_summary.json` — headline, level, trend, change sentence
- `data/processed/wwscan_covid.csv` — the full series
