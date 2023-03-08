jobs {
    rsconnectDeployPkgdown {
        jobDescription = 'job deploys pkgdown websites to RStudio Connect'
        jobType = 'PIPELINE_JOB'
        pipelineDefinitionFile = 'jenkins/rsconnect_deploy_pkgdown.groovy'
    }
    blackDuckScan {
        jobDescription = 'job to scan black-duck'
        jobType = 'PIPELINE_JOB'
        pipelineDefinitionFile = 'jenkins/black_duck_scan.groovy'  
    }
}
folders {
    CHECK_PACKAGE {
        jobs {
            "checkPackage" {
                jobDescription = 'job runs check package and code coverage for all repo branches'
                jobType = 'MULTIBRANCH_JOB'
                pipelineDefinitionFile = 'jenkins/check_package.groovy'
            }
        }
    }
}