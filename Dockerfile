## From https://gist.github.com/hasufell/f0893abfbba63ac4ea40feb0520946ee
#FROM alpine:3.14.0 AS builder
#
## prepare build dir
#WORKDIR /app
#
#ENV GHC_VERSION=8.10.7
## ENV GHC_OPTIONS="'-threaded -rtsopts -with-rtsopts=-N -split-sections -O2'"
#
## FIXME: CABAL NOT STACK
## install ghc and stack
#RUN \
#	apk add curl gcc g++ gmp-dev ncurses-dev libffi-dev make xz tar perl zlib zlib-dev && \
#	curl https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > /usr/bin/ghcup && \
#	chmod +x /usr/bin/ghcup && \
#	ghcup install ghc ${GHC_VERSION} && \
#	ghcup set ghc ${GHC_VERSION} && \
#	ghcup install stack
#
#COPY Setup.hs Setup.hs
#COPY hie.yaml hie.yaml
#COPY stack.yaml stack.yaml
#COPY stack.yaml.lock stack.yaml.lock
#COPY src src
#COPY swapi.cabal swapi.cabal
#COPY swoogle swoogle
#
#RUN \
#	export PATH="/root/.ghcup/bin:$PATH" && \
#	stack build swapi:exe:swoogle \
#      --system-ghc \
#      --stack-yaml=stack.yaml \
#      --no-nix \
#      --copy-bins \
#      --verbose

# ============================================================================ #

FROM alpine:3.14.0 AS assets

ENV TAILWIND_VERSION=3.0.18
ENV TAILWIND_URL=https://github.com/tailwindlabs/tailwindcss/releases/download/v${TAILWIND_VERSION}/tailwindcss-linux-x64

ENV ESBUILD_VERSION=0.14.13
ENV ESBUILD_URL=https://registry.npmjs.org/esbuild-linux-64/-/esbuild-linux-64-${ESBUILD_VERSION}.tgz

WORKDIR /app

# Download Tailwind
RUN \
    apk add curl && \
    curl -sL ${TAILWIND_URL} -o /usr/bin/tailwindcss && \
     chmod +x /usr/bin/tailwindcss

# Download esbuild
RUN \
    apk add curl && \
    curl ${ESBUILD_URL} | tar xvz && \
     mv ./package/bin/esbuild /usr/bin/esbuild && \
     chmod +x /usr/bin/esbuild

# Copy swoogle template
# Need this because Tailwind purges the classes in the template. So, without it,
# only the default styles will remain.
COPY swoogle swoogle

# Copy swoogle static files
COPY assets assets
COPY priv/static/images priv/static/images

RUN apk add tree

RUN tree .

# Build and minify stylesheet
RUN \
    mkdir priv/static/assets && \
    tailwindcss \
      --input assets/app.css \
      --output priv/static/assets/app.css \
      --config assets/tailwind.config.js \
      --minify

# Build and minify *S
RUN \
    esbuild assets/app.js \
      --outfile=priv/static/assets/app.js \
      --minify

# ============================================================================ #

FROM alpine:3.14.0

WORKDIR /app

RUN chown nobody /app
RUN apk add gcc g++ gmp-dev ncurses-dev libffi-dev make xz tar perl zlib zlib-dev

COPY --chown=nobody:root swoogle-server swoogle
COPY --from=assets --chown=nobody:root /app/priv priv

EXPOSE 3000

CMD ["/app/swoogle"]
