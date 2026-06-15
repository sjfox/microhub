FROM --platform=linux/amd64 rocker/shiny:4.5.1

ENV DEBIAN_FRONTEND=noninteractive \
    WORKON_HOME=/opt/virtualenvs \
    RETICULATE_PYTHON=/opt/virtualenvs/fourcat_env/bin/python \
    PYTHONUNBUFFERED=1 \
    OPENBLAS_NUM_THREADS=1 \
    OMP_NUM_THREADS=1 \
    MKL_NUM_THREADS=1

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    cmake \
    gfortran \
    git \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libgdal-dev \
    libgeos-dev \
    libgit2-dev \
    libgsl-dev \
    libharfbuzz-dev \
    libjpeg-dev \
    libpng-dev \
    libproj-dev \
    libssl-dev \
    libtiff5-dev \
    libudunits2-dev \
    libxml2-dev \
    pandoc \
    python3 \
    python3-pip \
    python3-venv \
    unzip \
    zip \
  && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages('pak', repos = 'https://cloud.r-project.org')" \
  && R -e "pak::pkg_install(c('dplyr', 'readr', 'lubridate', 'tidyr', 'purrr', 'forcats', 'tibble', 'stringr', 'ggplot2', 'cowplot', 'scales', 'shiny', 'shinyjs', 'bslib', 'DT', 'markdown', 'sn', 'cmu-delphi/epiprocess@main', 'reichlab/simplets', 'mgcv', 'gam', 'MMWRweek', 'lightgbm', 'slider', 'reticulate'), ask = FALSE, upgrade = FALSE)" \
  && R -e "install.packages('fmesher', repos = c(inlabruorg = 'https://inlabru-org.r-universe.dev', CRAN = 'https://cloud.r-project.org'), dependencies = c('Depends', 'Imports', 'LinkingTo'))" \
  && R -e "install.packages('INLA', repos = c(INLA = 'https://inla.r-inla-download.org/R/stable', CRAN = 'https://cloud.r-project.org'), dependencies = c('Depends', 'Imports', 'LinkingTo'))" \
  && R -e "library(INLA); stopifnot(packageVersion('fmesher') >= '0.5.0')"

RUN python3 -m venv /opt/virtualenvs/fourcat_env \
  && /opt/virtualenvs/fourcat_env/bin/python -m pip install --no-cache-dir --upgrade pip \
  && /opt/virtualenvs/fourcat_env/bin/python -m pip install --no-cache-dir \
    torch==2.2.2 \
    pandas==2.2.3 \
    numpy==1.26.4 \
  && chmod -R a+rX /opt/virtualenvs

RUN rm -rf /srv/shiny-server/*

WORKDIR /srv/shiny-server

COPY . /srv/shiny-server

RUN mkdir -p /srv/shiny-server/figures /srv/shiny-server/output /srv/shiny-server/tmp \
  && find /usr/local/lib/R/site-library/INLA/bin -type f -name "inla*" -exec chmod a+rx {} \; \
  && chmod +x /srv/shiny-server/data/sirsea2 || true \
  && chown -R shiny:shiny /srv/shiny-server

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
