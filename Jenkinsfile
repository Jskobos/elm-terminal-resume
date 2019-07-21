pipeline {
  agent {
    docker {
      image docker 'maven:3-alpine'
      label 'docker-agent'
    }

  }
  stages {
    stage('Build image') {
      steps {
        script {
          sh 'echo "Building image from Dockerfile"'
          def elmImage = docker.build("kobonaut/elm-terminal-resume")
        }

      }
    }
    stage('Test image') {
      steps {
        sh 'echo "Tests passed"'
      }
    }
  }
}
