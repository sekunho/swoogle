# From https://gist.github.com/hasufell/f0893abfbba63ac4ea40feb0520946ee
FROM alpine:3.14.0 AS builder

# prepare build dir
WORKDIR /app

ENV GHC_VERSION=8.10.7

# Need -fPIC otherwise `stack` will fail to build.
# https://stackoverflow.com/questions/41419102/haskell-stack-static-binary-relocation-r-x86-64-32-against-tmc-end-can-not
# ENV GHC_OPTS="-static -optl-static -optl-pthread -fPIC -threaded -rtsopts -with-rtsopts=-N"

# install ghc and stack
RUN \
	apk add git curl gcc g++ gmp-dev ncurses-dev libffi-dev make xz tar perl zlib zlib-dev && \
	curl https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > /usr/bin/ghcup && \
	chmod +x /usr/bin/ghcup && \
	ghcup install ghc ${GHC_VERSION} && \
	ghcup set ghc ${GHC_VERSION} && \
	ghcup install stack

COPY Setup.hs Setup.hs
COPY hie.yaml hie.yaml
COPY stack.yaml stack.yaml
COPY stack.yaml.lock stack.yaml.lock
COPY swapi-lib swapi-lib
COPY swapi.cabal swapi.cabal
COPY swoogle swoogle

RUN \
	export PATH="/root/.ghcup/bin:$PATH" && \
	stack build swapi:exe:swoogle \
      --system-ghc \
      --stack-yaml=stack.yaml \
      --copy-bins

# ============================================================================ #

FROM alpine:3.14.0

WORKDIR /app

RUN chown nobody /app
RUN apk add gcc g++ gmp-dev ncurses-dev libffi-dev make xz tar perl zlib zlib-dev

COPY --from=builder --chown=nobody:root /root/.local/bin/swoogle swoogle-bin
COPY --from=builder --chown=nobody:root /app/swoogle/priv/static/ swoogle/priv/static

EXPOSE 3000

CMD ["/app/swoogle-bin"]
