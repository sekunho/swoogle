FROM utdemir/ghc-musl:v22-ghc921 as builder

# prepare build dir
WORKDIR /app

COPY . .

RUN cabal update
RUN cabal build swapi:exe:swoogle --enable-executable-static

FROM alpine:3.14.0

WORKDIR /app
RUN chown nobody /app

COPY --from=builder --chown=nobody:root /app/swoogle/swoogle ./
COPY --from=builder --chown=nobody:root /app/swoogle/swoogle/priv/static/ ./

RUN tree .

USER nobody

EXPOSE 3000

CMD ["/app/swoogle"]
