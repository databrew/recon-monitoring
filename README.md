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

1. User make changes on the shiny modules to adjust based on customer use case and desired visuals on the dashboard

2. Code-review

3. Once code is reviewed and finalized do pre-deployment by running:

```r
golem::add_dockerfile_with_renv(output_dir = "deploy")
```

This command will create all the assets related to deploying Shiny App to Dockerhub under /deploy folder. Specifically it will create the required: 

- `.gz` extension files: Binary for the Golem Package

- `renv.lock.prod` the renv.lock used for production environment

- `Dockerfile_base` is what is the required package installation listed in Dockerfile

- `Dockerfile` combines `.gz` binary, `Renv` environment, and `base Dockerfile` to create all the deployment environment to successfully deploy this app into ECS Containers

Check this [youtube video](https://www.youtube.com/watch?v=5kynbRXEkEY&ab_channel=arytontediarjo) for walkthrough

### FAQs:

**Why ECS over EC2?**

- ECS is chosen in this process as how it can visualize the deployment in the console
- Assuming using lowest tier, ECS process is slightly more expensive but it provides higher capacity in user concurrency
- Isolated and can be terminated easily by console admins when project has been delivered

**What happens if the app overloads?**

- ShinyApps are being deployed as a [ECS Service](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs_services.html)
- In the scenario that the app is overloaded w/ CPU or memory, Shiny App will re-deploy itself to get itself to minimum one running instance
- We will work proactively to add in more instance with load balancers as needed if Shiny Apps breaks in high frequency due to CPU/memory overload







