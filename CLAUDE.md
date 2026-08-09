# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

An R pipeline that scrapes MWRA's Biobot COVID wastewater PDF, extracts the data tables, writes CSVs, generates static plots, and updates a JS dashboard published to GitHub Pages. Runs twice daily via GitHub Actions. State (the last known sample date) is committed to the repo, so runs are stateless between CI invocations.

Since August 2026 there is a **second, parallel pipeline** (`run_wwscan.R` + `R/05_wwscan.R`) reading WastewaterSCAN's feed for the same Deer Island sewershed. It exists because MWRA's Biobot publishing stalled for 19 days in July 2026. The two pipelines share only `R/utils.R`; they have separate state files, CSVs, workflows, and charts.

This is **not an R package** — there is no `DESCRIPTION` or `NAMESPACE`. Everything is plain scripts `source()`d into the global environment. That single fact drives most of the testing conventions below.

## Commands

Run from the repo root — every script uses paths relative to it.

```r
# MWRA Biobot: check -> download -> extract -> visualize -> update dashboard data
source("run_monitor.R")

# WastewaterSCAN: fetch -> extract -> CSV + summary JSON -> dashboard data
source("run_wwscan.R")
```

Running `run_wwscan.R` locally needs curl-impersonate, which isn't on Windows. Point it at plain curl instead — the WastewaterSCAN bucket has no bot wall:

```sh
CURL_IMPERSONATE_BIN=curl Rscript run_wwscan.R
```

To force a run when MWRA hasn't published new data, set `force <- TRUE` at the top of `run_monitor.R` before sourcing. Note that a successful run **rewrites tracked files** (`data/processed/*.csv`, `output/plots/*.png`, `state/last_update.json`, `docs/data/`) — check `git status` afterward.

```sh
# Full test suite
Rscript run_tests.R
```

```r
# A single test file (from an R session with the repo root as cwd)
testthat::test_file("tests/testthat/test-utils.R")
```

`test_file()` auto-loads `tests/testthat/helper-setup.R`, which locates the project root and sources all of `R/` in dependency order, so single files run standalone.

**`tests/testthat.R` does not work** — it's a vestigial R-package-style harness calling `test_check("biobot_mwra")`, which fails with "there is no package called 'biobot_mwra'". Use `run_tests.R`. Don't "fix" it by creating a DESCRIPTION; see the testing constraint below.

```r
# View the dashboard locally — it fetches CSV over HTTP, so file:// will not work
servr::httd("docs")
```

There is no build step and no linter configured.

## Architecture

**Pipeline stages.** `run_monitor.R` sources and calls these in order; the numeric prefixes in `R/` are execution order, not just naming:

1. `R/01_check_updates.R` — `check_for_updates()` scrapes `mwra.com/biobot/biobotdata.htm` for the sample date and PDF link, compares against `state/last_update.json` to decide if data is new.
2. `R/02_download_pdf.R` — `download_pdf()` fetches the PDF and validates the `%PDF-` magic bytes before accepting it.
3. `R/03_extract_data.R` — `extract_pdf_data()` parses the PDF text layout with regex (a date plus 8 numeric columns per row) into north/south/combined data frames; `extract_and_save()` writes `data/processed/*.csv`.
4. `R/04_visualize.R` — `generate_all_plots()` builds ggplot2 bar + error-bar charts (90-day North/South) into `output/plots/`.
5. `run_monitor.R` then copies `combined_data.csv` into `docs/data/` and calls `update_state()`.

`R/utils.R` holds the cross-cutting pieces: state load/save, `with_retry()` (exponential backoff + jitter), and the fetch/challenge-detection layer.

**The MWRA bot wall is the central design constraint.** MWRA sits behind an Imperva/Incapsula challenge that intermittently serves an interstitial page instead of real content — to both the page scrape and the PDF download. `impersonate_fetch()` shells out to `curl-impersonate` (patched curl with a Chrome TLS fingerprint) rather than using httr2/libcurl, because plain libcurl traffic gets fingerprinted and blocked. `is_bot_challenge()` detects the interstitial by signature strings.

A challenge is treated as a **transient no-op** — clean exit, no alarm — *unless* the newest processed data is older than `MAX_STALE_DAYS` (14, in `run_monitor.R`), in which case the run fails loudly. See `handle_challenge()` in `run_monitor.R` and `data_staleness_days()` in `R/utils.R`. Any change to fetch or retry logic must preserve this distinction between "challenged, retry next run" and "genuinely broken, wake a human." `last_sample_date` advances only on a fully successful run, which is what makes the staleness check survive fresh CI checkouts — no per-run counter would.

**`impersonate_fetch()` is the only network entry point** in the codebase. Every network-touching test stubs it. Keep it that way; adding a second fetch path would silently escape both the challenge handling and the test seams.

**Container-based CI.** `check-data.yml` installs nothing at run time — it executes inside a prebuilt image (`Dockerfile`, pushed to GHCR by `build-image.yml`, which triggers only on `Dockerfile` changes). The image pins `rocker/r-ver:4.4.2`, installs packages from a dated Posit Package Manager snapshot (`2026-06-02`), and bakes in curl-impersonate v0.6.1 / chrome116. This exists because fresh per-run installs intermittently produced an `rlang.so: undefined symbol: SETLENGTH` ABI crash. After editing the `Dockerfile`, manually run the **Build container image** workflow before the next scheduled check, or that run may fail pulling a stale image (self-heals on the following run).

**Dashboard** (`docs/index.html`): a single self-contained HTML/CSS/JS file using ECharts 5.4.3 from CDN, calling `fetch('data/combined_data.csv')`. That `fetch` is why it needs a real web server locally. No build step. Deployed by the `deploy-pages` job in `check-data.yml`, which re-deploys `docs/` from `main` after `check-data`.

**GitHub Actions handoff**: `run_monitor.R` writes `data_updated` and `sample_date` to `$GITHUB_OUTPUT` via `set_gha_output()`. `check-data.yml` gates the commit/push, issue creation, and artifact upload on those values. `run_monitor.R` also emits `data_stale`/`stale_days` when MWRA has published nothing for over `MAX_STALE_DAYS`, which files one `stale-data` issue (guarded by an open-issue check, so it never repeats). Free-text outputs reach `github-script` through `env:`, never string interpolation.

## The WastewaterSCAN pipeline

`R/05_wwscan.R` + `run_wwscan.R` + `.github/workflows/check-wwscan.yml`, sharing `R/utils.R` with the MWRA side.

**Never merge the two data series.** Biobot reports copies/mL of wastewater (flow-adjusted); WastewaterSCAN reports gene copies per gram dry solids normalized to PMMoV. Same virus, same sewershed, incomparable numbers. They get separate CSVs, separate charts, and an explicit caveat on the dashboard.

**The feed is an undocumented internal endpoint.** `https://storage.googleapis.com/wastewater-dev-data/json/<plant-id>.json` is what the dashboard's own JS fetches; there is no public API. It could move without notice, so `run_wwscan.R` fails loudly (rather than going quietly stale) when the fetch breaks or the newest sample passes `MAX_STALE_DAYS`. Fetching goes through `impersonate_fetch()` — keep it that way, per the single-network-entry-point rule above.

**Level and trend intentionally reproduce the WastewaterSCAN dashboard's own output**, so Sharon's email can never contradict their site. Two pieces of reverse-engineered knowledge make that work, both verified against the dashboard on 2026-08-08 ("SARS-CoV-2 Medium / No trend in the last 21 days and medium concentration"):

- The plotted value is `gc_g_dry_weight_trimmed5_pmmov * 1e6`; CI fields are the `_pmmov_lci`/`_uci` pair, which belong to the *unsmoothed* value.
- `WWSCAN_TERTILE_33`/`_66` are national percentile cutoffs hardcoded in the dashboard's JS bundle, not served as data. The per-sample `activity_category` field in the JSON is a *different*, plant-relative metric and will often disagree — don't substitute it for the level.
- The trend regresses the **unsmoothed** series over 21 days. Regressing the smoothed series reports a significant trend nearly every week (it's autocorrelated by construction) and does not match the dashboard.

If any of these need re-checking, the README documents how to re-derive them from the `/_nuxt/*.js` bundles.

**`docs/data/wwscan_summary.json` is the single source of truth for the headline**, consumed by both the dashboard JS and the server email script. Don't reimplement the tertile cutoffs or the slope test in JavaScript or on the server.

**`docs/data/wwscan_summary.json` and `data/processed/wwscan_covid.csv` are a published interface**, not private dashboard files — they are fetched over HTTP from `raw.githubusercontent.com` by a consumer outside this repo. Renaming their fields or paths breaks that consumer silently, and nothing in this repo's test suite will catch it. Treat a change to either as a breaking change.

**This file is public.** It is committed to a public GitHub repo, so keep deployment specifics, server paths, hostnames, and anything else environment-specific out of it. Those live in `.claude/DEPLOYMENT.md`, which is gitignored — read that file when working on anything deployment-related.

## Testing conventions

Tests were largely generated by the Posit `r-lib:testing-r-packages` skill and, per the README, have not been rigorously reviewed. Match the existing style rather than importing package-testing idioms:

- **All tests use `describe()` / `it()` BDD style.** There is not a single `test_that()` call in the suite.
- **No snapshot tests and no `Config/testthat/edition`** (no DESCRIPTION exists to hold one). Fixtures are plain HTML files in `tests/testthat/fixtures/` (`challenge_page.html`, `normal_page.html`).
- **Mocking is done by monkey-patching the global environment** — `assign("impersonate_fetch", stub, envir = .GlobalEnv)` with restoration via `on.exit()`. This works *only* because the project `source()`s functions into globalenv instead of a package namespace. Converting this repo to a package, or moving functions into a namespace, would silently break every mock. `withr::local_tempdir()` handles filesystem isolation.
- The suite is fully offline — `with_retry` is stubbed to fail fast or to run its closure once, so there are no real requests and no sleeps.

- `test-wwscan.R` stubs both `impersonate_fetch` and `with_retry` via `local_wwscan_stubs()`, and uses `tests/testthat/fixtures/wwscan_boston.json` — a trimmed copy of the real feed (last 30 samples, two targets). Its newest sample is 2026-08-05, so the summary tests double as a regression check that we still match the dashboard's wording.

**Known pre-existing failures (as of 2026-08-08): 236 pass, 2 fail.** Both are in `test-visualize.R` (lines ~55 and ~66) and assert the old palette — North `#2166AC`, South `#B2182B` — while `R/04_visualize.R:67` now uses North `#1a5d1a` (green) and South `#2166ac`. The tests are stale, not the code. Don't treat these as a regression you caused; if you touch plot colors, update these expectations to match.
