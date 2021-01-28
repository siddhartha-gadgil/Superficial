FROM gitpod/workspace-full

RUN sudo sh -c '(echo "#!/usr/bin/env sh" && curl -L https://github.com/lihaoyi/Ammonite/releases/download/2.3.8/2.13-2.3.8) > /usr/local/bin/amm && chmod +x /usr/local/bin/amm'

RUN sudo curl -L https://github.com/lihaoyi/mill/releases/download/0.9.5/0.9.5 > /usr/local/bin/mill && sudo chmod +x /usr/local/bin/mill

RUN brew install scala coursier/formulas/coursier sbt scalaenv

RUN sudo env "PATH=$PATH" coursier bootstrap org.scalameta:scalafmt-cli_2.13:4.4.7 \
  -r sonatype:snapshots \
  -o /usr/local/bin/scalafmt --standalone --main org.scalafmt.cli.Cli

RUN scalaenv install scala-2.13.4 && scalaenv global scala-2.13.4
