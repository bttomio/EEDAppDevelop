# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest

## Install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libssl-dev \
    libudunits2-dev

## Update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean


# Copy required files
## Empty directory to which azure will mount the drake cache
COPY /Cache_Folder ./Cache_Folder
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

# Install renv & restore packages
RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::restore()'

# Expose port
EXPOSE 3838

# Run app on container start
CMD ["R", "-e", "shiny::runApp('app.R', host = '0.0.0.0', port = 3838)"]
