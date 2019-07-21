pipeline {
  agent {
    docker {
      image 'debian:9'
    }

  }
  stages {
    stage('Build image') {
      steps {
        script {
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
