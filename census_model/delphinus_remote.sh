#### Stan
## copy stan data to delphinus
scp ./census_model/Stan/2023_stan_data.json mike@delphinus:/home/mike/census21/STAN/2023_stan_data.json
## copy stan model to delphinus
scp ./census_model/Stan/census_model.stan mike@delphinus:/home/mike/census21/STAN/census_model.stan

## Compile model
ssh mike@delphinus \
  "nohup \
      docker run --rm \
      -v /home/mike/census21/STAN:/STAN \
      -w /STAN \
      census_tag \
      Rscript -e 'cmdstanr::cmdstan_model(\"census_model.stan\", compile = TRUE)' \
      > /home/mike/census21/STAN/stan_compile.log 2>&1 &"

## copy stan model run script to delphinus
scp ./census_model/Stan/2023_run_stan.R mike@delphinus:/home/mike/census21/STAN/2023_run_stan.R
## Run model
ssh mike@delphinus \
  " nohup \
    docker run --rm \
    -v /home/mike/census21/STAN:/STAN \
    -w /STAN \
    census_tag \
    Rscript -e 'source(\"2023_run_stan.R\")' \
    > /home/mike/census21/STAN/out2023.log 2>&1 &"



#### 2022
### local to remote
# copy jags model to delphinus
scp ./census_model/census_model_2022.jags mike@delphinus:/home/mike/census21/census_model_2022.jags

# copy model setup to delphinus
scp ./census_model/2022_model.R mike@delphinus:/home/mike/census21/2022_model.R

# Run container
ssh mike@delphinus \
  " nohup \
    docker run --rm \
    -v /home/mike/census21:/census_model \
    -w /census_model \
    census21:latest \
    Rscript -e 'source(\"2022_model.R\")' \
    > /home/mike/census21/out2022.log 2>&1 &"


# Update
ssh mike@delphinus \
  " nohup \
    docker run --rm \
    -v /home/mike/census21:/census_model \
    -w /census_model \
    census21:latest \
    Rscript -e 'library(jagsUI);m<-readRDS(\"out2022_3.RDS\");m2<-update(m, n.iter=20000);saveRDS(m2, file=\"out2022_4.RDS\")' \
    >> /home/mike/census21/out2022.log 2>&1 &"

### Remote to local
# copy results from delphinus to local
scp mike@delphinus:/home/mike/census21/out2022_4.RDS ./census_model


#### 2023
### local to remote
# copy jags model to delphinus
scp ./census_model/JAGS/census_model_2023.jags mike@delphinus:/home/mike/census21/

# copy model setup to delphinus
scp ./census_model/2023_model.R mike@delphinus:/home/mike/census21/2023_model.R

# Run container
ssh mike@delphinus \
  " nohup \
    docker run --rm \
    -v /home/mike/census21:/census_model \
    -w /census_model \
    census21:latest \
    Rscript -e 'source(\"2023_model.R\")' \
    > /home/mike/census21/out2023_1Septstart.log 2>&1 &"


# Check in
ssh mike@delphinus "cat census21/out2023.log; docker ps"


# Update
ssh mike@delphinus \
  " nohup \
    docker run --rm \
    -v /home/mike/census21:/census_model \
    -w /census_model \
    census21:latest \
    Rscript -e 'library(jagsUI);m<-readRDS(\"out2023_4.RDS\");m2<-update(m, n.iter=20000);saveRDS(m2, file=\"out2023_5.RDS\")' \
    >> /home/mike/census21/out2023.log 2>&1 &"

### Remote to local
# copy results from delphinus to local
scp mike@delphinus:/home/mike/census21/out2023_3.RDS ./census_model
scp mike@delphinus:/home/mike/census21/out2023_4.RDS ./census_model
scp mike@delphinus:/home/mike/census21/out2023_5.RDS ./census_model

