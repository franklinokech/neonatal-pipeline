FROM rocker/tidyverse:4.5.2

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libuv1-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy renv files
COPY renv.lock ./
COPY .Rprofile ./
COPY renv/ ./renv/

# Install renv and restore packages
RUN R -e "install.packages('renv', repos='https://cran.rstudio.com/')" && \
    R -e "renv::restore()"

COPY scripts/ ./scripts/

RUN mkdir -p /app/logs /app/cache

ENTRYPOINT ["Rscript", "scripts/main.R"]