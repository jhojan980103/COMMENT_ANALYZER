FROM rocker/shiny:4.2.2

RUN apt-get update && apt-get install \
  libcurl4-openssl-dev \
  libv8-dev \
  curl -y \
  libpq-dev \
  libharfbuzz-dev \
  libfribidi-dev \
  libxml2-dev

RUN mkdir -p /var/lib/shiny-server/bookmarks/shiny

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