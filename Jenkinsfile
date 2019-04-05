pipeline {
    agent any
    
    environment {
        PROJECT_NAME = 'RCB4Cloud'
        NAMESPACE = 'trait-analytics'
    }

    parameters {
        choice(choices: 'non-prod\nprod', description: 'What environment?', name: 'environment')
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
    }
    post {
        success {
            sh 'docker image rm rcb:latest'
        }
    }
}
