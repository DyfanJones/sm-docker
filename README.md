
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sm-docker

<!-- badges: start -->

[![R-CMD-check](https://github.com/DyfanJones/sm-docker/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/DyfanJones/sm-docker/actions/workflows/R-CMD-check.yaml)
[![smdocker status
badge](https://dyfanjones.r-universe.dev/badges/smdocker)](https://dyfanjones.r-universe.dev)
[![Codecov test
coverage](https://codecov.io/gh/DyfanJones/sm-docker/branch/main/graph/badge.svg)](https://app.codecov.io/gh/DyfanJones/sm-docker?branch=main)
<!-- badges: end -->

Building Docker images in SageMaker Studio using AWS CodeBuild.

## Installation

r-universe installation:

``` r
# Install smdocker in R:
install.packages(
  'smdocker',
  repos = c('https://dyfanjones.r-universe.dev', 'https://cloud.r-project.org')
)
```

Github installation

``` r
remotes::install_github("dyfanjones/sm-docker")
```

## Example:

Execute `smdocker` on current directory.

``` r
smdocker::sm_build()
```

Execute `smdocker` on different directory.

``` r
smdocker::sm_build(dir="my-project")
```

Additional arguments for `docker build` are supported. However using
“\_” instead of “-” for example: `build-arg` -\> `build_arg`

``` r
smdocker::sm_build(
  file = "/path/to/Dockerfile",
  build_arg = "foo=bar"
)
```

Similar to python’s `sm-docker`. By default, the CodeBuild project will
not run within a VPC, the image will be pushed to a repository
sagemakerstudio with the tag latest, and use the Studio App’s execution
role and the default SageMaker Python SDK S3 bucket.

These can be overridden:

``` r
smdocker::sm_build(
  repository = "mynewrepo:1.0",
  role = "SampleDockerBuildRole",
  bucket = "sagemaker-us-east-1-326543455535",
  vpc_id = "vpc-0c70e76ef1c603b94",
  subnet_ids = list("subnet-0d984f080338960bb", "subnet-0ac3e96808c8092f2"),
  security_group_ids = list("sg-0d31b4042f2902cd0")
)
```

## Execution role requirements:

**NOTE:** Execution role example taken from
<https://github.com/aws-samples/sagemaker-studio-image-build-cli#installing>

Ensure the execution role has a trust policy with CodeBuild.

``` json
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Principal": {
        "Service": [
          "codebuild.amazonaws.com"
        ]
      },
      "Action": "sts:AssumeRole"
    }
  ]
}
```

The following permissions are required in the execution role to execute
a build in CodeBuild and push the image to ECR.

``` json
{
    "Version": "2012-10-17",
    "Statement": [
        {
            "Effect": "Allow",
            "Action": [
                "codebuild:DeleteProject",
                "codebuild:CreateProject",
                "codebuild:BatchGetBuilds",
                "codebuild:StartBuild"
            ],
            "Resource": "arn:aws:codebuild:*:*:project/sagemaker-studio*"
        },
        {
            "Effect": "Allow",
            "Action": "logs:CreateLogStream",
            "Resource": "arn:aws:logs:*:*:log-group:/aws/codebuild/sagemaker-studio*"
        },
        {
            "Effect": "Allow",
            "Action": [
                "logs:GetLogEvents",
                "logs:PutLogEvents"
            ],
            "Resource": "arn:aws:logs:*:*:log-group:/aws/codebuild/sagemaker-studio*:log-stream:*"
        },
        {
            "Effect": "Allow",
            "Action": "logs:CreateLogGroup",
            "Resource": "*"
        },
        {
            "Effect": "Allow",
            "Action": [
                "ecr:CreateRepository",
                "ecr:BatchGetImage",
                "ecr:CompleteLayerUpload",
                "ecr:DescribeImages",
                "ecr:DescribeRepositories",
                "ecr:UploadLayerPart",
                "ecr:ListImages",
                "ecr:InitiateLayerUpload", 
                "ecr:BatchCheckLayerAvailability",
                "ecr:PutImage"
            ],
            "Resource": "arn:aws:ecr:*:*:repository/sagemaker-studio*"
        },
        {
            "Sid": "ReadAccessToPrebuiltAwsImages",
            "Effect": "Allow",
            "Action": [
                "ecr:BatchGetImage",
                "ecr:GetDownloadUrlForLayer"
            ],
            "Resource": [
                "arn:aws:ecr:*:763104351884:repository/*",
                "arn:aws:ecr:*:217643126080:repository/*",
                "arn:aws:ecr:*:727897471807:repository/*",
                "arn:aws:ecr:*:626614931356:repository/*",
                "arn:aws:ecr:*:683313688378:repository/*",
                "arn:aws:ecr:*:520713654638:repository/*",
                "arn:aws:ecr:*:462105765813:repository/*"
            ]
        },
        {
            "Sid": "EcrAuthorizationTokenRetrieval",
            "Effect": "Allow",
            "Action": [
                "ecr:GetAuthorizationToken"
            ],
            "Resource": [
                "*"
            ]
        },
        {
            "Effect": "Allow",
            "Action": [
              "s3:GetObject",
              "s3:DeleteObject",
              "s3:PutObject"
              ],
            "Resource": "arn:aws:s3:::sagemaker-*/*"
        },
        {
            "Effect": "Allow",
            "Action": [
                "s3:CreateBucket"
            ],
            "Resource": "arn:aws:s3:::sagemaker*"
        },
        {
            "Effect": "Allow",
            "Action": [
                "iam:GetRole",
                "iam:ListRoles"
            ],
            "Resource": "*"
        },
        {
            "Effect": "Allow",
            "Action": "iam:PassRole",
            "Resource": "arn:aws:iam::*:role/*",
            "Condition": {
                "StringLikeIfExists": {
                    "iam:PassedToService": "codebuild.amazonaws.com"
                }
            }
        }
    ]
}
```

If you need to run your CodeBuild project within a VPC, please add the
following actions to your execution role that the CodeBuild Project will
assume:

            {
                "Sid": "VpcAccessActions",
                "Effect": "Allow",
                "Action": [
                    "ec2:CreateNetworkInterface",
                    "ec2:CreateNetworkInterfacePermission",
                    "ec2:DescribeDhcpOptions",
                    "ec2:DescribeNetworkInterfaces",
                    "ec2:DeleteNetworkInterface",
                    "ec2:DescribeSubnets",
                    "ec2:DescribeSecurityGroups",
                    "ec2:DescribeVpcs"
                ],
                "Resource": "*"
            }
