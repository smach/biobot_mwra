# biobot_mwra — operational notes

## MWRA source is behind an intermittent Imperva (Incapsula) bot challenge

The entire `www.mwra.com` domain — both the HTML index page
(`/biobot/biobotdata.htm`) **and** the dated PDF files
(`/media/file/mwradataYYYYMMDD-datapdf`) — sits behind an Imperva
JavaScript challenge ("Please wait while your request is being
verified...") that is served **intermittently**. When it's active, no
plain HTTP client can get through — including `curl-impersonate` with any
TLS fingerprint — because the challenge must be solved by *executing
JavaScript*.

- The scheduled workflow (`.github/workflows/check-data.yml`, twice daily)
  therefore fails roughly ~30% of the time and succeeds the rest.
  Failures self-heal on the next run. This is expected, not a regression.
- Bumping the curl-impersonate fingerprint does nothing once the challenge
  is active (Chrome 116 and Chrome 146 both get challenged identically).

## Branch `claude/github-action-failure-analysis-kminxz` (PR #40) — NOT a confirmed fix

Reworks `check_for_updates()` to skip the HTML page and probe the dated
PDF URLs directly (`build_pdf_url()` + `impersonate_http_status()` +
`is_pdf_file()` magic-byte check). **A 2026-06-30 run on this branch
proved the PDF endpoint is also behind the same Imperva challenge**, so
this is NOT a reliable bypass. Keep it for the cleaner code and clear
diagnostics, but do not expect it to improve reliability over `main`.
Look here again if failures get problematic — but the durable fix is
below, not this branch.

## The only durable fix is executing the JS challenge

A headless browser (Playwright/Chromium, or R's `chromote`) that loads the
page, lets Imperva set its clearance cookie, then reads the real content /
downloads the PDF in the same session. Larger change; pursue only if the
intermittent failures become unacceptable. Note: a datacenter (GitHub
Actions) IP may still get escalated to a CAPTCHA even with a real browser.

## Notification emails

"New data" emails are GitHub issue notifications from the workflow's
"Create notification issue" step, which runs only when
`data_updated == 'true'` (i.e. a newer report than last time was found and
committed). No new data → no issue → no email. If emails stop, first
confirm whether MWRA actually posted a newer report and whether recent
runs detected it (intermittent failures can delay detection). Last
data-update issue as of 2026-06-30: #39 (2026-06-22 data).
