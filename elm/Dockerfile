FROM codesimple/elm:0.18 as builder

RUN mkdir /root/build
COPY *.elm /root/build/
WORKDIR /root/build/
RUN mkdir static-content
RUN elm-package install --yes elm-lang/http
RUN elm-make BattleShip.elm --yes --output static-content/index.html

FROM nginx
COPY --from=builder /root/build/static-content/* /usr/share/nginx/html/
COPY *.conf /etc/nginx/conf.d/
