FROM ubuntu:18.04


RUN apt-get -y update
RUN apt-get -y install make python
RUN apt-get -y install curl libreadline-dev libedit-dev

RUN mkdir -p /mal
WORKDIR /mal

RUN apt-get -y install ocaml-batteries-included ocaml-dune rlwrap
