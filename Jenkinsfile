@Library("youcruit-jenkinslib") _

pipeline {
    options {
        buildDiscarder(
            logRotator(
                numToKeepStr: '30',
                artifactDaysToKeepStr: '20',
                artifactNumToKeepStr: '30',
                daysToKeepStr: '20',
            )
        )
        timestamps()
    }

    agent {
        label 'youcruitbuilder'
    }

    environment {
        PROJECT = "form-ml-projects"
    }

    stages {
        stage('Checkout') {
            options {
                timeout(time: 2, unit: 'MINUTES')
            }
            steps {
                checkout scm
            }
        }
        stage('VersionCheck') {
            steps {
                versionCheck()
            }
        }
        stage("Build and publish") {
            options {
                timeout(time: 30, unit: 'MINUTES')
            }
            environment {
                NPM_ARTIFACTORY_AUTH = credentials("NPM_ARTIFACTORY_AUTH")
                NPM_ARTIFACTORY_EMAIL = "teknik+jenkins@youcruit.com"
            }
            steps {
                configFileProvider([configFile(fileId: 'org.jenkinsci.plugins.configfiles.custom.CustomConfig1448492140289', targetLocation: "${env.HOME}/.ssh/id_rsa")]) {
                    script {
                        sh 'chmod 0600 "$HOME"/.ssh/id_rsa'
                        sh 'ci-scripts/build_pipeline.sh'
                    }
                }
            }
        }
    }
    post {
        always {
            script {
                def parent = "master"
                if (env.BRANCH_NAME == "master") {
                    parent = "master"
                } else if (env.BRANCH_NAME == "release") {
                    parent = "master"
                } else if (env.BRANCH_NAME == "develop") {
                    parent = "release"
                }

                recordIssues (
                    ignoreQualityGate: true,
                    enabledForFailure: true,
                    qualityGates: [[threshold: 1, type: 'NEW', unstable: true]],
                    referenceJobName: "YouCruit-CI/${PROJECT}/${parent}",
                    tools: [
                        jsLint(pattern: '**/jslint.xml')
                    ]
                )
                // This works around a Jenkins issue regarding not finding tests due to create date
                sh 'find . -name "*.xml" -print0 | xargs -0 touch'
                junit(testResults: '**/test-report.xml', allowEmptyResults: false)
                archiveArtifacts artifacts: '**/test-report.xml', fingerprint: false, allowEmptyArchive: true
            }
        }
    }
}
