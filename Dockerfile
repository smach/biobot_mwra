# Reproducible runtime for the MWRA Biobot data pipeline.
#
# Building the whole R environment once, pinned, is what permanently fixes the
# intermittent `rlang.so: undefined symbol: SETLENGTH` crash that came from
# installing r-cran-* binaries fresh (against a moving target) on every run.
# curl-impersonate is baked in too, so runs no longer depend on downloading it
# from GitHub releases each time.
FROM rocker/r-ver:4.4.2

# System libraries the pipeline's R packages link against: poppler (pdftools),
# libxml2/openssl/curl (xml2, rvest, httr2), plus font/image libs (ggplot2).
RUN apt-get update && apt-get install -y --no-install-recommends \
      libpoppler-cpp-dev \
      libxml2-dev \
      libssl-dev \
      libcurl4-openssl-dev \
      libfontconfig1-dev \
      libfreetype6-dev \
      libpng-dev \
      libjpeg-dev \
      ca-certificates \
      curl \
      git \
    && rm -rf /var/lib/apt/lists/*

# Install packages from a dated Posit Public Package Manager snapshot so every
# rebuild is deterministic and internally consistent. The Ubuntu codename is
# read from the base image so the correct binary repo is used automatically.
# rocker/r-ver's Rprofile.site already sets the binary-package User-Agent PPM
# needs, so these resolve to prebuilt binaries (fast, no compilation).
RUN . /etc/os-release \
    && REPO="https://packagemanager.posit.co/cran/__linux__/${UBUNTU_CODENAME}/2026-06-02" \
    && R -q -e "install.packages(c('rvest','httr2','pdftools','dplyr','readr','ggplot2','scales','jsonlite','testthat','withr','xml2'), repos='${REPO}')" \
    && R -q -e "pkgs <- c('rvest','httr2','pdftools','dplyr','readr','ggplot2','scales','jsonlite','testthat','withr','xml2'); missing <- pkgs[!pkgs %in% rownames(installed.packages())]; if (length(missing)) { stop('Missing packages: ', paste(missing, collapse=', ')) }"

# Bake in curl-impersonate (Chrome TLS fingerprint) used to clear MWRA's
# Imperva bot wall. Kept at the proven v0.6.1 / chrome116 build; the pipeline
# now also tolerates the occasional challenge gracefully. To cut the challenge
# rate further, bump this to a newer fingerprint build.
RUN mkdir -p /opt/curl-impersonate \
    && curl -fsSL https://github.com/lwthiker/curl-impersonate/releases/download/v0.6.1/curl-impersonate-v0.6.1.x86_64-linux-gnu.tar.gz \
       | tar -xz -C /opt/curl-impersonate \
    && chmod +x /opt/curl-impersonate/curl_* /opt/curl-impersonate/curl-impersonate-* \
    && /opt/curl-impersonate/curl_chrome116 --version | head -n1
ENV PATH="/opt/curl-impersonate:${PATH}"

WORKDIR /work
