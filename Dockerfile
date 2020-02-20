FROM ocaml/opam2

LABEL maintainer = "jsbzwork@gmail.com"

##### Dockerfile for Development
# This docker image provides minimal environment to build MicSE.

### Update apt and install packages
RUN sudo apt-get update -y -qq \
    # m4 for ocamlfind dependency
    # libgmp-dev for zarith dependency
    && sudo apt-get install -y -qq m4 libgmp-dev

### Set OCaml to 4.07.01
RUN opam switch 4.07

### Update opam and install opam packages
RUN opam update -y -q \
    # build tools
    && opam install -y -q dune \
    # project dependencies
    && opam install -y -q batteries zarith \
    # Setup environment variables
    && eval $(opam env)

### Miscellaneous, customizable settings. Should be erased for release version.
RUN sudo apt-get install vim -y -qq \
    && echo 'alias ll="ls -al"' >> ~/.bashrc

### Set the project directory
ENV projdir /home/opam/MicSE
RUN sudo mkdir -p ${projdir}
WORKDIR ${projdir}
COPY . ${projdir}
# Change ownership to resolve permission error in shell environment.
RUN sudo chown -R opam ${projdir} \
    && sudo chgrp -R opam ${projdir}

### Entrypoint
CMD ["/bin/bash"]
