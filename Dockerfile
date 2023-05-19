FROM rocker/shiny:4.2.2

# Bibliotecas del sistema de uso general
RUN apt-get update && apt-get install \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libigraph0-dev \
    libglu1-mesa-dev \
    libx11-dev \
    libudunits2-dev \
    libpoppler-cpp-dev \
    libtesseract-dev \
    libleptonica-dev \
    libprotobuf-dev \
    protobuf-compiler \
    libjq-dev \
    libv8-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libmagick++-dev

# Instalar paquetes R
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'readxl', 'rstudioapi', 'dplyr', 'tidytext', \
                             'janeaustenr', 'tm', 'tidyr', 'wordcloud', 'wordcloud2', 'ggplot2', 'igraph', \
                             'ggraph', 'tictoc', 'Rtsne', 'factoextra', 'DT', 'plotly', 'ggwordcloud', \
                             'imager', 'png', 'shinycssloaders', 'ClusterR', 'rgl', 'syuzhet', 'reshape2', \
                             'SnowballC', 'stringr', 'tidyverse', 'openxlsx'), \
                             repos='http://cran.rstudio.com/')"

# Copiar el app a la imagen de shinyapps /srv/shiny-server/
COPY . /srv/shiny-server/
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

RUN chown shiny:shiny /srv/shiny-server/

# Configurar permisos en caso de que sea desarrollado desde windows
RUN chmod -R 755 /srv/shiny-server/

EXPOSE 8080