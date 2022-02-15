# From https://gist.github.com/hasufell/f0893abfbba63ac4ea40feb0520946ee
FROM alpine:3.14.0 as builder

# prepare build dir
WORKDIR /app

COPY . .

ENV GHC_VERSION=8.10.7

# Need -fPIC otherwise `stack` will fail to build.
# https://stackoverflow.com/questions/41419102/haskell-stack-static-binary-relocation-r-x86-64-32-against-tmc-end-can-not
ENV GHC_OPTS="-static -optl-static -optl-pthread -fPIC -threaded -rtsopts -with-rtsopts=-N"

# install ghc and stack
RUN \
	apk add --no-cache git curl gcc g++ gmp-dev ncurses-dev libffi-dev make xz tar perl && \
	apk add --no-cache zlib zlib-dev zlib-static ncurses-static && \
	curl https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > /usr/bin/ghcup && \
	chmod +x /usr/bin/ghcup && \
	ghcup install ghc ${GHC_VERSION} && \
	ghcup set ghc ${GHC_VERSION} && \
	ghcup install stack

RUN \
	export PATH="/root/.ghcup/bin:$PATH" && \
	stack build swapi:exe:swoogle \
      --system-ghc \
      --stack-yaml=stack.yaml \
      --ghc-options="${GHC_OPTS}" \
      --copy-bins


FROM alpine:3.14.0

WORKDIR /app

RUN chown nobody /app

COPY --from=builder --chown=nobody:root /root/.local/bin/swoogle .
COPY --from=builder --chown=nobody:root /app/swapi/swoogle/priv/static/ ./swoogle/priv/static/

EXPOSE 3000

CMD ["/app/swoogle"]
