FROM rocker/shiny:4.1.0

# Remove boilerplate.
RUN rm -rf /srv/shiny-server/

# install preliminary requirements
RUN apt-get update -y\
    && apt-get install -y git

# clone github repository
RUN git clone https://github.com/databrew/recon-monitoring.git /root/recon-monitoring

# use the bohemia kenya work directory
WORKDIR /root/recon-monitoring

# Copy app.
RUN mv app.R /srv/shiny-server/app.R
