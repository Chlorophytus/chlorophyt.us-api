FROM erlang:22-alpine

ADD . /opt/chlorophytus
WORKDIR /opt/chlorophytus
RUN rebar3 release -d false
CMD ["/opt/chlorophytus/_build/default/rel/chlorophytus/bin/chlorophytus-0.2.0.2", "console"]
