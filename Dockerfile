FROM rocker/shiny:latest

# System-level dependencies
RUN apt-get update && apt-get install -y \
  libcurl4-openssl-dev \
  libssl-dev \
  libxml2-dev \
  libgit2-dev \
  libharfbuzz-dev \
  libfribidi-dev \
  libfreetype6-dev \
  libpng-dev \
  libtiff5-dev \
  libjpeg-dev \
  zlib1g-dev \
  libglpk-dev \
  libgsl-dev \
  libhdf5-dev \
  libudunits2-dev \
  libgeos-dev \
  libgdal-dev \
  libv8-dev \
  pandoc \
  && rm -rf /var/lib/apt/lists/*

# Install required R packages
RUN R -e "install.packages(c( \
  'shiny', 'bslib', 'shinyjs', 'ggplot2', 'ggsignif', 'DT', 'tidyr', 'dplyr', \
  'vegan', 'SummarizedExperiment', 'plotly', 'googledrive', 'testthat', 'shinytest2' \
))"

# Install Bioconductor packages
RUN R -e "if (!requireNamespace('BiocManager', quietly = TRUE)) install.packages('BiocManager'); \
          BiocManager::install(c('lefser', 'Maaslin2'))"

# Copy your app files
COPY . /srv/shiny-server/

# Set working directory
WORKDIR /srv/shiny-server/

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', port=3838, host='0.0.0.0')"]
