FROM debian:testing

RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y locales

RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
    dpkg-reconfigure --frontend=noninteractive locales && \
    update-locale LANG=en_US.UTF-8

ENV LANG en_US.UTF-8 

COPY entangled_0.2.0.0_amd64.deb .
RUN apt-get install -y ./entangled_0.2.0.0_amd64.deb

WORKDIR /tmp
