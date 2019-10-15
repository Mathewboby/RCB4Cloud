pipeline {
    agent any
    
    environment {
        PROJECT_NAME = 'RCB4Cloud'
        NAMESPACE = 'trait-analytics'
    }

    parameters {
        choice(choices: 'non-prod\nprod\ndev', description: 'What environment?', name: 'environment')
    }

    stages {
        stage('Non Production Build') {
            when {
                expression { params.environment == 'non-prod' }
            }
            steps {
                sh 'cd docker && ./build.sh np'
            }
        }
        stage('Production Build') {
            when {
                expression { params.environment == 'prod' }
            }
            steps {
                sh 'cd docker && ./build.sh prod'
            }
        }
        stage('Development Build') {
            when {
                expression { params.environment == 'dev' }
            }
            steps {
                sh 'cd docker && ./build.sh dev'
            }
        }
    }
    post {
        success {
            sh 'docker image rm rcb:latest'
        }
    }
}
