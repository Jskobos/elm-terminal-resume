FROM codesimple/elm

COPY ./src /app
RUN elm make /app/Main.elm

FROM nginx:alpine
COPY --from=0 /app /usr/share/nginx/html