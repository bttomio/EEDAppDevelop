# Base image https://hub.docker.com/u/rocker/
FROM zekemarshall/EEDAppBaseImage:latest

## Install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libssl-dev \
    libudunits2-dev

## Update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# Test Section - Add sshd_config
## Install OpenSSH and set the password for root to "Docker!". In this example, "apk add" is the install instruction for an Alpine Linux-based image.
RUN apt-get install openssh \
     && echo "root:Docker!" | chpasswd

## Copy the sshd_config file to the /etc/ssh/ directory
COPY sshd_config /etc/ssh/

## Open port 2222 for SSH access
EXPOSE 80 2222

# Copy required files
## app file
COPY /app.R /app.R
## App modules folder
COPY /App-Modules /App-Modules
## Database documentation
COPY /datadoctest.Rmd /datadoctest.Rmd
## Database documentation bibliography
COPY /databasedocumentation.bib /databasedocumentation.bib
## Rebound documentation
COPY /reboundtools_doc.Rmd /reboundtools_doc.Rmd
## www folder
COPY /www /www


# Expose port
EXPOSE 3838

# Run app on container start
CMD ["R", "-e", "shiny::runApp('app.R', host = '0.0.0.0', port = 3838)"]
