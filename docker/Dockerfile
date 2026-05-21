FROM rocker/r-ver:4.5

RUN apt-get update && apt-get install -y \
    lighttpd \
    libcurl4-openssl-dev libssl-dev libxml2-dev \
    cmake libavfilter-dev libicu-dev libjpeg-dev libuv1-dev libx11-dev zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('remotes', 'shiny', 'shinyWidgets', 'dplyr', 'DT', \
  'digest', 'fs', 'future', 'future.apply', 'htmltools', 'assertthat', \
  'base64enc', 'rappdirs', 'rlang', 'servr', 'sys'))"

RUN R -e "install.packages('datavolley', repos = c('https://openvolley.r-universe.dev', 'https://cloud.r-project.org'))" && R -e "stopifnot('datavolley' %in% installed.packages()[, 'Package'])"
RUN R -e "install.packages('ovideo', repos = c('https://openvolley.r-universe.dev', 'https://cloud.r-project.org'))" && R -e "stopifnot('ovideo' %in% installed.packages()[, 'Package'])"
RUN R -e "install.packages('peranavolley', repos = c('https://openvolley.r-universe.dev', 'https://cloud.r-project.org'))" && R -e "stopifnot('peranavolley' %in% installed.packages()[, 'Package'])"

COPY . /app/ovva
RUN sed -i '/^Remotes:/d' /app/ovva/DESCRIPTION
RUN R CMD INSTALL /app/ovva
RUN R -e "stopifnot('ovva' %in% installed.packages()[, 'Package'])"

COPY docker/entrypoint.R /entrypoint.R
EXPOSE 3838 8888
CMD ["Rscript", "/entrypoint.R"]
