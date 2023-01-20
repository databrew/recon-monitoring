# Recon Monitoring

Author: atediarjo@gmail.com

## Description

Dashboard used for Recon Monitoring. It is used for recon tracking and reports:

Here are all the reports being shown in the dashboard
- Progress
- Anomalies
- Fieldworker Performance
- Internet Connectivity

## Prerequisites

Using this dashboard will require password from DataBrew internal team, please contact atediarjo@gmail.com  / joebrew@gmail.com for authentication

## Contributing

### Running Locally
```r
devtools::install_github("databrew/recon-monitoring")
```
After package is installed locally and you are authenticated to access our AWS resources, run:
```r
reconmonitoring::run_app()
```

### Deployment

**Resources used**:
- Service in AWS ECS: Container microservice that hosts docker container (in our case Shiny Apps) from Dockerhub
- CloudWatch Insights: To track load of the shiny apps
- Github Actions: CI to integrate changes to Dockerhub

**AWS Credentials Usage**:
We are using AWS IAM roles in our instance, no need to pass in credentials yaml file

**Process Flow**:
1. User make changes on the R scripts
2. Code-review & performance optimization
3. Once code is reviewed and finalized:
```r
golem::add_dockerfile_with_renv(output_dir = "deploy")
```





