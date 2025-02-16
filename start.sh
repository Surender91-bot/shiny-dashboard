#!/bin/bash
# Install R and Shiny Server
sudo apt update && sudo apt install -y r-base
R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
R -e "shiny::runApp(host='0.0.0.0', port=8080)"
