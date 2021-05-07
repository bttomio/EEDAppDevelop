# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    # Required for R package "units"
    libudunits2-dev



## Update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# Copy necessary files

## renv.lock file
COPY /renv.lock ./renv.lock
## app file
COPY /app.R /app.R
## App modules folder
COPY /App-Modules /App-Modules
## Database documentation
COPY /datadoctest.Rmd /datadoctest.Rmd
## Database documentation bibliography
COPY /databasedocumentation.bib /databasedocumentation.bib
## www folder
COPY /www /www

# install renv & restore packages
RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::restore()'

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('app.R', host = '0.0.0.0', port = 3838)"]
