# Base R Shiny image
FROM rocker/shiny

# Make a directory in the container
RUN mkdir /home/shiny-app

# Install R dependencies
RUN R -e "install.packages(c('bslib','DT','tidyverse','factoextra','Rtsne','class','stats','caret','cluster','clusterCrit','dbscan','caTools','MASS', 'ggplot2'))"

# Copy the Shiny app code
COPY app.R /home/shiny-app/app.R

# Expose the application port
EXPOSE 8180

# Run the R Shiny app
CMD Rscript /home/shiny-app/app.R