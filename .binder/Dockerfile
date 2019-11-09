FROM rocker/binder:3.6.1
LABEL maintainer='Ate Poorthuis'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}



RUN wget https://github.com/atepoorthuis/containment-connectivity-urban-systems/raw/master/DESCRIPTION && R -e "options(repos = list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2019-11-09/')); devtools::install_deps()"

RUN rm DESCRIPTION.1; exit 0
