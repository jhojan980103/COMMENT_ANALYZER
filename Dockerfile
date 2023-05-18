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

# Instalar paquete remotes para controlar las versiones de otros paquetes
RUN R -e 'install.packages("remotes", repos="http://cran.rstudio.com")'

# Descargar e instalar paquetes de R necesarios para el app
RUN R -e 'remotes::install_version(package = "shiny", version = "1.7.4", dependencies = TRUE)'
RUN R -e 'remotes::install_version(package = "tm", version = "0.7-11")'
RUN R -e 'remotes::install_version(package = "igraph", version = "1.4.2")'
RUN R -e 'remotes::install_version(package = "SnowballC", version = "0.7.0", dependencies = TRUE)'
RUN R -e 'remotes::install_version(package = "wordcloud", version = "2.6", dependencies = TRUE)'
RUN R -e 'remotes::install_version(package = "shinydashboard", version = "0.7.2", dependencies = TRUE)'
RUN R -e 'remotes::install_version(package = "ggplot2", version = "3.4.1", dependencies = TRUE)'
RUN R -e 'remotes::install_version(package = "dplyr", version = "1.1.0", dependencies = TRUE)'
RUN R -e 'remotes::install_version(package = "readxl", version = "1.4.2", dependencies = TRUE)'
RUN R -e 'remotes::install_version(package = "rstudioapi", version = "0.14", dependencies = TRUE)'
RUN R -e 'remotes::install_version(package = "tidytext", version = "0.4.1", dependencies = TRUE)'
RUN R -e 'remotes::install_version(package = "janeaustenr", version = "1.0.0", dependencies = TRUE)'
RUN R -e 'remotes::install_version(package = "tidyr", version = "1.3.0", dependencies = TRUE)'
RUN R -e 'remotes::install_version(package = "wordcloud2", version = "0.2.1", dependencies = TRUE)'
RUN R -e 'remotes::install_version(package = "ggraph", version = "2.1.0", dependencies = TRUE)'
RUN R -e 'remotes::install_version(package = "tictoc", version = "1.1", dependencies = TRUE)'
RUN R -e 'remotes::install_version(package = "Rtsne", version = "0.16", dependencies = TRUE)'
RUN R -e 'remotes::install_version(package = "factoextra", version = "1.0.7", dependencies = TRUE)'
RUN R -e 'remotes::install_version(package = "DT", version = "0.27", dependencies = TRUE)'
RUN R -e 'remotes::install_version(package = "plotly", version = "4.10.1", dependencies = TRUE)'
RUN R -e 'remotes::install_version(package = "ggwordcloud", version = "0.5.0", dependencies = TRUE)'
RUN R -e 'remotes::install_version(package = "imager", version = "0.42.18", dependencies = TRUE)'
RUN R -e 'remotes::install_version(package = "png", version = "0.1-8", dependencies = TRUE)'
RUN R -e 'remotes::install_version(package = "shinycssloaders", version = "1.0.0", dependencies = TRUE)'
RUN R -e 'remotes::install_version(package = "ClusterR", version = "1.3.0", dependencies = TRUE)'
RUN R -e 'remotes::install_version(package = "rgl", version = "1.0.1", dependencies = TRUE)'
RUN R -e 'remotes::install_version(package = "syuzhet", version = "1.0.6", dependencies = TRUE)'
RUN R -e 'remotes::install_version(package = "reshape2", version = "1.4.4", dependencies = TRUE)'
RUN R -e 'remotes::install_version(package = "stringr", version = "1.5.0", dependencies = TRUE)'
RUN R -e 'remotes::install_version(package = "tidyverse", version = "2.0.0", dependencies = TRUE)'
RUN R -e 'remotes::install_version(package = "openxlsx", version = "4.2.5.2", dependencies = TRUE)'
RUN R -e 'remotes::install_version(package = "reticulate", version = "1.28", dependencies = TRUE)'

# Copiar el app a la imagen de shinyapps /srv/shiny-server/
COPY . /srv/shiny-server/
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

RUN chown shiny:shiny /srv/shiny-server/

# Configurar permisos en caso de que sea desarrollado desde windows
RUN chmod -R 755 /srv/shiny-server/

EXPOSE 8080