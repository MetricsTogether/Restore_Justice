# get shiny server plus tidyverse packages image
FROM rocker/shiny-verse:latest

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
	xtail \
	wget

# http://127.0.0.1:5419/

# install R packages required 
    
# (change it depending on the packages you need)
RUN R -e "install.packages(c('shinydashboard', 'tidyverse', 'lubridate', 'stringi', 'readxl', 'censusapi', 'tigris', 'sf', 'DT'), repos='http://cran.rstudio.com/')"


# Copy configuration files into the Docker image
COPY shiny-server.conf  /usr/bin/shiny-server/shiny-server.conf
COPY app.R /srv/shiny-server/
RUN rm /srv/shiny-server/index.html

# Make the ShinyApp available at port 80
EXPOSE 80

# Copy further configuration files into the Docker image
COPY shiny-server.sh /usr/bin/shiny-server.sh

RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]
CMD ["/usr/bin/shiny-server.sh"]