FROM debian:jessie
COPY . /code
RUN \
  apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442 && \
  echo deb http://download.fpcomplete.com/debian jessie main > /etc/apt/sources.list.d/fpco.list && \
  apt-get update && \
  apt-get install --assume-yes stack && \
  cd /code && \
  stack --local-bin-path /usr/local/bin build --copy-bins && \
  cd - && \
  rm -r /code && \
  rm -r $(stack path --global-stack-root) && \
  apt-get purge --assume-yes stack && \
  apt-get autoremove --assume-yes --purge && \
  apt-get autoclean --assume-yes
ENV SERVER_HOST *
ENV SERVER_PORT 80
EXPOSE 80
CMD blunt
