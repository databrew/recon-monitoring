# Recon Monitoring

Author: atediarjo@gmail.com

## Description

Dashboard used for Recon Monitoring. It is used for recon tracking and reports:

Here are all the reports being shown in the recon monitoring dashboard:

- Progress

- Anomalies

- Fieldworker Performance

- Internet Connectivity

## Prerequisites

- **Dashboard Authentication**: Dashboard will require password from DataBrew internal team, please contact atediarjo@gmail.com  / joebrew@gmail.com / Bohemia sponsors for authentication

- **AWS Authentication (Developers)**: Request atediarjo@gmail.com for SSO access to AWS and refer to this authentication video when you have received email invitation to get AWS access from our team

## Contributing

To install this golem package run:
```r
devtools::install_github("databrew/recon-monitoring")
```

After package has been installed locally and you are authenticated to access our AWS resources, run:
```r
reconmonitoring::run_app()
```

### Deployment

**AWS Credentials Usage**:

We are using AWS IAM roles in our instance, no need to pass in credentials yaml file

**Process Flow**:

1. User make changes on the R scripts

2. Code-review & performance optimization

3. Once code is reviewed and finalized do pre-deployment by running:

```r
golem::add_dockerfile_with_renv(output_dir = "deploy")
```

This command will create all the assets related to deploying Shiny App to Dockerhub


### Deployment Options
More details here in [Golem deployment guide](https://cran.r-project.org/web/packages/golem/vignettes/c_deploy.html)

#### Option 1: DataBrew Shiny Server (AWS EC2)
TBD Video

#### Option 2: DataBrew Shiny Cluster (AWS ECS)
TBD Video






