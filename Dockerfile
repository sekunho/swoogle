# From https://gist.github.com/hasufell/f0893abfbba63ac4ea40feb0520946ee
FROM alpine:3.14.0 AS builder

# prepare build dir
WORKDIR /app

ENV GHC_VERSION=8.10.7

# install ghc and stack
RUN \
	apk add curl gcc g++ gmp-dev ncurses-dev libffi-dev make xz tar perl zlib zlib-dev && \
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
      --no-nix \
      --copy-bins

FROM alpine:3.14.0 AS tailwind

ENV TAILWIND_VERSION=3.0.18
ENV TAILWIND_URL=https://github.com/tailwindlabs/tailwindcss/releases/download/v${TAILWIND_VERSION}/tailwindcss-linux-x64

WORKDIR /app

# Download Tailwind
RUN \
    apk add curl && \
    curl -sL ${TAILWIND_URL} -o /usr/bin/tailwindcss && \
     chmod +x /usr/bin/tailwindcss

# Copy swoogle template
# Need this because Tailwind purges the classes in the template. So, without it,
# only the default styles will remain.
COPY swoogle swoogle

# Copy swoogle static files
COPY assets assets
COPY priv/static/fonts priv/static/fonts
COPY priv/static/images priv/static/images

# Build and minify stylesheet
RUN \
    mkdir priv/static/assets && \
    tailwindcss \
      --input assets/app.css \
      --output priv/static/assets/app.css \
      --config assets/tailwind.config.js \
      --minify

# ============================================================================ #

FROM alpine:3.14.0

WORKDIR /app

RUN chown nobody /app
RUN apk add gcc g++ gmp-dev ncurses-dev libffi-dev make xz tar perl zlib zlib-dev

COPY --from=builder --chown=nobody:root /root/.local/bin/swoogle swoogle
COPY --from=tailwind --chown=nobody:root /app/priv/ priv/

EXPOSE 3000

CMD ["/app/swoogle"]
