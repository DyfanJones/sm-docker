test_that("check get_role default", {
  mock_sagemaker_get_execution_role <- mock2(
    "arn:aws:iam::012345678910:role/made.up"
  )
  mockery::stub(
    get_role, "sagemaker_get_execution_role", mock_sagemaker_get_execution_role
  )
  actual <- get_role()
  expect_equal(
    actual, "made.up"
  )
})

test_that("check get_role with role", {
  actual <- get_role("dummy")
  expect_equal(actual, "dummy")
})

test_that("check get_role default", {
  mock_sagemaker_get_caller_identity_arn <- mock2(
    "arn:aws:iam::012345678910:role/made.up"
  )
  mockery::stub(
    sagemaker_get_execution_role,
    "sagemaker_get_caller_identity_arn",
    mock_sagemaker_get_caller_identity_arn
  )
  actual <- sagemaker_get_execution_role()
  expect_equal(
    actual, "arn:aws:iam::012345678910:role/made.up"
  )
})

test_that("check get_role bad role", {
  mock_sagemaker_get_caller_identity_arn <- mock2(
    "arn:aws:iam::012345678910:iam/made.up"
  )
  mockery::stub(
    sagemaker_get_execution_role,
    "sagemaker_get_caller_identity_arn",
    mock_sagemaker_get_caller_identity_arn
  )
  expect_error(
    sagemaker_get_execution_role(),
    "The current AWS identity is not a role:"
  )
})

test_that("check sagemaker_get_caller_identity_arn describe_user_profile", {
  mock_smdocker_config <- mock2()
  mock_file_exists <- mock2(TRUE)

  # mock sagemaker client
  mock_describe_user_profile <- mock2(
    list(
      "DomainId" = "string",
      "UserProfileArn" = "string",
      "UserProfileName" = "string",
      "HomeEfsFileSystemUid" = "string",
      "Status" = "InService",
      "LastModifiedTime" = as.POSIXct("2023-01-01"),
      "CreationTime" = as.POSIXct("2023-01-01"),
      "FailureReason" = "string",
      "SingleSignOnUserIdentifier" = "string",
      "SingleSignOnUserValue" = "string",
      "UserSettings" = list(
        "ExecutionRole" = "foo:bar:role",
        "SecurityGroups" = list(
          "string"
        ),
        "SharingSettings" = list(
          "NotebookOutputOption" = "Allowed",
          "S3OutputPath" = "string",
          "S3KmsKeyId" = "string"
        ),
        "JupyterServerAppSettings" = list(
          "DefaultResourceSpec" = list(
            "SageMakerImageArn" = "string",
            "SageMakerImageVersionArn" = "string",
            "InstanceType" = "system",
            "LifecycleConfigArn" = "string"
          ),
          "LifecycleConfigArns" = list(
            "string"
          ),
          "CodeRepositories" = list(
            list(
              "RepositoryUrl" = "string"
            )
          )
        ),
        "KernelGatewayAppSettings" = list(
          "DefaultResourceSpec" = list(
            "SageMakerImageArn" = "string",
            "SageMakerImageVersionArn" = "string",
            "InstanceType" = "system",
            "LifecycleConfigArn" = "string"
          ),
          "CustomImages" = list(
            list(
              "ImageName" = "string",
              "ImageVersionNumber" = 123,
              "AppImageConfigName" = "string"
            )
          ),
          "LifecycleConfigArns" = list(
            "string"
          )
        ),
        "TensorBoardAppSettings" = list(
          "DefaultResourceSpec" = list(
            "SageMakerImageArn" = "string",
            "SageMakerImageVersionArn" = "string",
            "InstanceType" = "system",
            "LifecycleConfigArn" = "string"
          )
        ),
        "RStudioServerProAppSettings" = list(
          "AccessStatus" = "ENABLED",
          "UserGroup" = "R_STUDIO_ADMIN"
        ),
        "RSessionAppSettings" = list(
          "DefaultResourceSpec" = list(
            "SageMakerImageArn" = "string",
            "SageMakerImageVersionArn" = "string",
            "InstanceType" = "system",
            "LifecycleConfigArn" = "string"
          ),
          "CustomImages" = list(
            list(
              "ImageName" = "string",
              "ImageVersionNumber" = 123,
              "AppImageConfigName" = "string"
            )
          )
        ),
        "CanvasAppSettings" = list(
          "TimeSeriesForecastingSettings" = list(
            "Status" = "ENABLED",
            "AmazonForecastRoleArn" = "string"
          )
        )
      )
    )
  )
  mock_sagemaker <- mock2(list(
    describe_user_profile = mock_describe_user_profile
  ))

  mock_read_json <- mock2(list(
    ResourceName = "foo",
    DomainId = "bar",
    UserProfileName = "cho"
  ))


  mockery::stub(
    sagemaker_get_caller_identity_arn, "smdocker_config", mock_smdocker_config
  )
  mockery::stub(
    sagemaker_get_caller_identity_arn, "file.exists", mock_file_exists
  )
  mockery::stub(
    sagemaker_get_caller_identity_arn, "sagemaker", mock_sagemaker
  )
  mockery::stub(
    sagemaker_get_caller_identity_arn, "read_json", mock_read_json
  )

  actual <- sagemaker_get_caller_identity_arn()

  expect_equal(actual, "foo:bar:role")
})

test_that("check sagemaker_get_caller_identity_arn describe_notebook_instance", {
  mock_smdocker_config <- mock2()
  mock_file_exists <- mock2(TRUE)

  # mock sagemaker client
  mock_describe_notebook_instance <- mock2(
    list(
      "NotebookInstanceArn" = "string",
      "NotebookInstanceName" = "string",
      "NotebookInstanceStatus" = "InService",
      "FailureReason" = "string",
      "Url" = "string",
      "InstanceType" = "ml.t2.medium",
      "SubnetId" = "string",
      "SecurityGroups" = list(
        "string"
      ),
      "RoleArn" = "foo:bar:role",
      "KmsKeyId" = "string",
      "NetworkInterfaceId" = "string",
      "LastModifiedTime" = as.POSIXct("2023-01-01"),
      "CreationTime" = as.POSIXct("2023-01-01"),
      "NotebookInstanceLifecycleConfigName" = "string",
      "DirectInternetAccess" = "Enabled",
      "VolumeSizeInGB" = 123,
      "AcceleratorTypes" = list(
        "ml.eia1.medium"
      ),
      "DefaultCodeRepository" = "string",
      "AdditionalCodeRepositories" = list(
        "string"
      ),
      "RootAccess" = "Enabled",
      "PlatformIdentifier" = "string",
      "InstanceMetadataServiceConfiguration" = list(
        "MinimumInstanceMetadataServiceVersion" = "string"
      )
    )
  )
  mock_sagemaker <- mock2(list(
    describe_notebook_instance = mock_describe_notebook_instance
  ))

  mock_read_json <- mock2(list(
    ResourceName = "foo",
    UserProfileName = "cho"
  ))


  mockery::stub(
    sagemaker_get_caller_identity_arn, "smdocker_config", mock_smdocker_config
  )
  mockery::stub(
    sagemaker_get_caller_identity_arn, "file.exists", mock_file_exists
  )
  mockery::stub(
    sagemaker_get_caller_identity_arn, "sagemaker", mock_sagemaker
  )
  mockery::stub(
    sagemaker_get_caller_identity_arn, "read_json", mock_read_json
  )

  actual <- sagemaker_get_caller_identity_arn()

  expect_equal(actual, "foo:bar:role")
})

test_that("check sagemaker_get_caller_identity_arn describe_domain", {
  mock_smdocker_config <- mock2()
  mock_file_exists <- mock2(TRUE)

  # mock sagemaker client
  mock_describe_notebook_instance <- mock2(
    list(
      "NotebookInstanceArn" = "string",
      "NotebookInstanceName" = "string",
      "NotebookInstanceStatus" = "InService",
      "FailureReason" = "string",
      "Url" = "string",
      "InstanceType" = "ml.t2.medium",
      "SubnetId" = "string",
      "SecurityGroups" = list(
        "string"
      ),
      "RoleArn" = "string",
      "KmsKeyId" = "string",
      "NetworkInterfaceId" = "string",
      "LastModifiedTime" = as.POSIXct("2023-01-01"),
      "CreationTime" = as.POSIXct("2023-01-01"),
      "NotebookInstanceLifecycleConfigName" = "string",
      "DirectInternetAccess" = "Enabled",
      "VolumeSizeInGB" = 123,
      "AcceleratorTypes" = list(
        "ml.eia1.medium"
      ),
      "DefaultCodeRepository" = "string",
      "AdditionalCodeRepositories" = list(
        "string"
      ),
      "RootAccess" = "Enabled",
      "PlatformIdentifier" = "string",
      "InstanceMetadataServiceConfiguration" = list(
        "MinimumInstanceMetadataServiceVersion" = "string"
      )
    )
  )
  mock_describe_user_profile <- mock2(
    list(
      "DomainId" = "string",
      "UserProfileArn" = "string",
      "UserProfileName" = "string",
      "HomeEfsFileSystemUid" = "string",
      "Status" = "InService",
      "LastModifiedTime" = as.POSIXct("2023-01-01"),
      "CreationTime" = as.POSIXct("2023-01-01"),
      "FailureReason" = "string",
      "SingleSignOnUserIdentifier" = "string",
      "SingleSignOnUserValue" = "string",
      "UserSettings" = list()
    )
  )
  mock_describe_domain <- mock2(
    list(
      "DomainArn" = "string",
      "DomainId" = "string",
      "DomainName" = "string",
      "HomeEfsFileSystemId" = "string",
      "SingleSignOnManagedApplicationInstanceId" = "string",
      "Status" = "InService",
      "CreationTime" = as.POSIXct("2023-01-01"),
      "LastModifiedTime" = as.POSIXct("2023-01-01"),
      "FailureReason" = "string",
      "AuthMode" = "SSO",
      "DefaultUserSettings" = list(
        "ExecutionRole" = "foo:bar:role",
        "SecurityGroups" = list(
          "string"
        ),
        "SharingSettings" = list(
          "NotebookOutputOption" = "Allowed",
          "S3OutputPath" = "string",
          "S3KmsKeyId" = "string"
        ),
        "JupyterServerAppSettings" = list(
          "DefaultResourceSpec" = list(
            "SageMakerImageArn" = "string",
            "SageMakerImageVersionArn" = "string",
            "InstanceType" = "system",
            "LifecycleConfigArn" = "string"
          ),
          "LifecycleConfigArns" = list(
            "string"
          ),
          "CodeRepositories" = list(
            list(
              "RepositoryUrl" = "string"
            )
          )
        ),
        "KernelGatewayAppSettings" = list(
          "DefaultResourceSpec" = list(
            "SageMakerImageArn" = "string",
            "SageMakerImageVersionArn" = "string",
            "InstanceType" = "system",
            "LifecycleConfigArn" = "string"
          ),
          "CustomImages" = list(
            list(
              "ImageName" = "string",
              "ImageVersionNumber" = 123,
              "AppImageConfigName" = "string"
            )
          ),
          "LifecycleConfigArns" = list(
            "string"
          )
        ),
        "TensorBoardAppSettings" = list(
          "DefaultResourceSpec" = list(
            "SageMakerImageArn" = "string",
            "SageMakerImageVersionArn" = "string",
            "InstanceType" = "system",
            "LifecycleConfigArn" = "string"
          )
        ),
        "RStudioServerProAppSettings" = list(
          "AccessStatus" = "ENABLED",
          "UserGroup" = "R_STUDIO_ADMIN"
        ),
        "RSessionAppSettings" = list(
          "DefaultResourceSpec" = list(
            "SageMakerImageArn" = "string",
            "SageMakerImageVersionArn" = "string",
            "InstanceType" = "system",
            "LifecycleConfigArn" = "string"
          ),
          "CustomImages" = list(
            list(
              "ImageName" = "string",
              "ImageVersionNumber" = 123,
              "AppImageConfigName" = "string"
            )
          )
        ),
        "CanvasAppSettings" = list(
          "TimeSeriesForecastingSettings" = list(
            "Status" = "ENABLED",
            "AmazonForecastRoleArn" = "string"
          )
        )
      ),
      "AppNetworkAccessType" = "PublicInternetOnly",
      "HomeEfsFileSystemKmsKeyId" = "string",
      "SubnetIds" = list(
        "string"
      ),
      "Url" = "string",
      "VpcId" = "string",
      "KmsKeyId" = "string",
      "DomainSettings" = list(
        "SecurityGroupIds" = list(
          "string"
        ),
        "RStudioServerProDomainSettings" = list(
          "DomainExecutionRoleArn" = "string",
          "RStudioConnectUrl" = "string",
          "RStudioPackageManagerUrl" = "string",
          "DefaultResourceSpec" = list(
            "SageMakerImageArn" = "string",
            "SageMakerImageVersionArn" = "string",
            "InstanceType" = "system",
            "LifecycleConfigArn" = "string"
          )
        ),
        "ExecutionRoleIdentityConfig" = "USER_PROFILE_NAME"
      ),
      "AppSecurityGroupManagement" = "Service",
      "SecurityGroupIdForDomainBoundary" = "string",
      "DefaultSpaceSettings" = list(
        "ExecutionRole" = "string",
        "SecurityGroups" = list(
          "string"
        ),
        "JupyterServerAppSettings" = list(
          "DefaultResourceSpec" = list(
            "SageMakerImageArn" = "string",
            "SageMakerImageVersionArn" = "string",
            "InstanceType" = "system",
            "LifecycleConfigArn" = "string"
          ),
          "LifecycleConfigArns" = list(
            "string"
          ),
          "CodeRepositories" = list(
            list(
              "RepositoryUrl" = "string"
            )
          )
        ),
        "KernelGatewayAppSettings" = list(
          "DefaultResourceSpec" = list(
            "SageMakerImageArn" = "string",
            "SageMakerImageVersionArn" = "string",
            "InstanceType" = "system",
            "LifecycleConfigArn" = "string"
          ),
          "CustomImages" = list(
            list(
              "ImageName" = "string",
              "ImageVersionNumber" = 123,
              "AppImageConfigName" = "string"
            )
          ),
          "LifecycleConfigArns" = list(
            "string"
          )
        )
      )
    )
  )
  mock_sagemaker <- mock2(list(
    describe_notebook_instance = mock_describe_notebook_instance,
    describe_user_profile = mock_describe_user_profile,
    describe_domain = mock_describe_domain
  ))

  mock_read_json <- mock2(list(
    ResourceName = "foo",
    DomainId = "bar",
    UserProfileName = "cho"
  ))


  mockery::stub(
    sagemaker_get_caller_identity_arn, "smdocker_config", mock_smdocker_config
  )
  mockery::stub(
    sagemaker_get_caller_identity_arn, "file.exists", mock_file_exists
  )
  mockery::stub(
    sagemaker_get_caller_identity_arn, "sagemaker", mock_sagemaker
  )
  mockery::stub(
    sagemaker_get_caller_identity_arn, "read_json", mock_read_json
  )

  actual <- sagemaker_get_caller_identity_arn()

  expect_equal(actual, "foo:bar:role")
})

test_that("check sagemaker_get_caller_identity_arn no NOTEBOOK_METADATA_FILE", {
  mock_smdocker_config <- mock2(list(region = "us-east-1"))
  mock_file_exists <- mock2(FALSE)

  # mock sagemaker client
  mock_sagemaker <- mock2()

  # mock sts client
  mock_get_caller_identity <- mock2(
    list(
      "Arn" = "arn:aws:sts::123456789:assumed-role/dumm-role/SageMaker"
    )
  )
  mock_sts <- mock2(list(
    get_caller_identity = mock_get_caller_identity
  ))


  # mock iam client
  mock_get_role <- mock2(
    list(
      Role = list(Arn = "foobar")
    )
  )
  mock_iam <- mock2(list(
    get_role = mock_get_role
  ))


  mockery::stub(
    sagemaker_get_caller_identity_arn, "smdocker_config", mock_smdocker_config
  )
  mockery::stub(
    sagemaker_get_caller_identity_arn, "file.exists", mock_file_exists
  )
  mockery::stub(
    sagemaker_get_caller_identity_arn, "sagemaker", mock_sagemaker
  )
  mockery::stub(
    sagemaker_get_caller_identity_arn, "sts", mock_sts
  )
  mockery::stub(
    sagemaker_get_caller_identity_arn, "iam", mock_iam
  )

  actual <- sagemaker_get_caller_identity_arn()

  expect_equal(actual, "foobar")
})


test_that("check sagemaker_get_caller_identity_arn no NOTEBOOK_METADATA_FILE execution role", {
  mock_smdocker_config <- mock2(list(region = "us-east-1"))
  mock_file_exists <- mock2(FALSE)

  # mock sagemaker client
  mock_sagemaker <- mock2()

  # mock sts client
  mock_get_caller_identity <- mock2(
    list(
      "Arn" = "arn:aws:sts::123456789:assumed-role/dumm-role/AmazonSageMaker-ExecutionRole"
    )
  )
  mock_sts <- mock2(list(
    get_caller_identity = mock_get_caller_identity
  ))


  # mock iam client
  mock_get_role <- mock2(
    stop(
      structure(list(message = "dummy error"), class = c("http_500", "error", "condition"))
    )
  )
  mock_iam <- mock2(list(
    get_role = mock_get_role
  ))


  mockery::stub(
    sagemaker_get_caller_identity_arn, "smdocker_config", mock_smdocker_config
  )
  mockery::stub(
    sagemaker_get_caller_identity_arn, "file.exists", mock_file_exists
  )
  mockery::stub(
    sagemaker_get_caller_identity_arn, "sagemaker", mock_sagemaker
  )
  mockery::stub(
    sagemaker_get_caller_identity_arn, "sts", mock_sts
  )
  mockery::stub(
    sagemaker_get_caller_identity_arn, "iam", mock_iam
  )

  actual <- sagemaker_get_caller_identity_arn()

  expect_equal(actual, "arn:aws:iam::123456789:role/dumm-role")
})
