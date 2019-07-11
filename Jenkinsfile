pipeline {
  agent any
  stages {
    stage('Dockerize') {
      steps {
        sh '''#!/bin/bash

docker build -t terminal-resume .
docker tag terminal-resume kobonaut/terminal-resume:latest
docker push kobonaut/terminal-resume:latest '''
      }
    }
  }
}