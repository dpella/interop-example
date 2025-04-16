FROM ubuntu:jammy

SHELL ["/bin/bash", "-c"]

# Set timezone
ENV TZ=Europe/Stockholm
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

# Install dependencies
RUN apt-get update && \
    export DEBIAN_FRONTEND=noninteractive && \
    apt-get install -y --no-install-recommends \
      sudo \
      git \
      curl \
      ca-certificates \
      locales \
      build-essential \
      libffi-dev \
      libgmp-dev \
      libncurses-dev \
      libsqlite3-dev \
      zlib1g-dev \
      libtinfo-dev \
      postgresql \
      postgresql-contrib \
      postgresql-server-dev-all \
      libpq-dev \
      mariadb-server \
      mariadb-client \
      libmariadb-dev \
      libmariadb-dev-compat \
      libmariadbd-dev \
      cmake \
      pkg-config \
      libssl-dev \
      libzstd-dev \
      libpcre3-dev \
      && \
    apt-get autoremove -y && \
    apt-get clean -y && \
    rm -rf /var/lib/apt/lists/*

# Enable utf-8 input
RUN echo "LC_ALL=en_US.UTF-8" >> /etc/environment
RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen
RUN echo "LANG=en_US.UTF-8" > /etc/locale.conf
RUN locale-gen en_US.UTF-8

# Setup a new user
ARG USER_NAME=dpella
ARG GHC_VERSION=9.6.5
ARG UID=1000
ARG GID=1000

RUN groupadd -g $GID -o $USER_NAME
RUN useradd -m -u $UID -g $GID -G sudo -o -s /bin/bash -d /home/$USER_NAME $USER_NAME
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers

# Switch to the new user
USER $USER_NAME
WORKDIR /home/$USER_NAME

# Install GHCup and GHC
ENV GHCUP_INSTALL_BASE_PREFIX=/home/$USER_NAME
ENV PATH=/home/$USER_NAME/.cabal/bin:/home/$USER_NAME/.ghcup/bin:$PATH

# Don't install anything automatically
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_NO_UPGRADE=1 \
    BOOTSTRAP_HASKELL_MINIMAL=1 \
    BOOTSTRAP_HASKELL_INSTALL=0

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
RUN ghcup install ghc $GHC_VERSION
RUN ghcup set ghc $GHC_VERSION
RUN ghcup install cabal latest

# Set environment variables
ENV LC_ALL=en_US.UTF-8

# Copy project files
WORKDIR /app
# The source code will be mounted at runtime

# Update cabal and run tests
RUN cabal update

COPY cabal.project .
COPY dpella-base ./dpella-base
COPY dpella-sqlite ./dpella-sqlite
COPY dpella-postgres ./dpella-postgres
COPY dpella-ffi ./dpella-ffi
COPY dpella-mysql ./dpella-mysql

USER root
RUN chown -R $USER_NAME:$USER_NAME /app

USER $USER_NAME

RUN cabal build all --only-dependencies

USER root
COPY example ./example
RUN chown -R $USER_NAME:$USER_NAME /app

USER $USER_NAME

# Add the ffi library to the project
RUN echo "packages: */*.cabal" > cabal.project.local
RUN cabal build all

USER root

# Copy all the libraries to /usr/local/lib/dpella
RUN mkdir -p /usr/local/lib/dpella
# Our new library
RUN cp $(find dist-newstyle -name '*libdpella-ffi.so*') /usr/local/lib/dpella/libdpella-ffi.so
# All the GHC libraries
RUN cp /home/$USER_NAME/.ghcup/ghc/$GHC_VERSION/lib/ghc-$GHC_VERSION/lib/x86_64-linux-ghc-$GHC_VERSION/*.so /usr/local/lib/dpella/
# All the dependencies
RUN find /home/$USER_NAME/.local/state/cabal/store/ghc-$GHC_VERSION -name '*.so' -exec cp {}  /usr/local/lib/dpella/ \; >> /etc/ld.so.conf.d/dpella.conf

# Update the dynamic linker cache to include the new libraries
RUN cat <<EOF > /etc/ld.so.conf.d/dpella.conf
/usr/local/lib/dpella
EOF
RUN ldconfig

# BUILD the postgres extension and mysql plugin
USER $USER_NAME
RUN cd /app/dpella-ffi/pg_extension && make && sudo make install
RUN cd /app/dpella-ffi/mysql_plugin && make && sudo make install

## Add PostgreSQL initialization script
COPY scripts ./scripts

USER root
RUN mkdir -p /docker-entrypoint-initdb.d
COPY --chown=postgres:postgres ./scripts/init-postgresql.sh /docker-entrypoint-initdb.d/
RUN chmod +x /docker-entrypoint-initdb.d/init-postgresql.sh
COPY --chown=mysql:mysql ./scripts/init-mysql.sh /docker-entrypoint-initdb.d/
RUN chmod +x /docker-entrypoint-initdb.d/init-mysql.sh

RUN cat <<EOF > /usr/local/bin/entrypoint
#!/bin/bash
sudo service postgresql start
/docker-entrypoint-initdb.d/init-postgresql.sh
sudo service mariadb start
/docker-entrypoint-initdb.d/init-mysql.sh
cabal run example 
EOF
RUN chmod +x /usr/local/bin/entrypoint


# Default command
USER $USER_NAME
ENTRYPOINT ["/usr/local/bin/entrypoint"]
CMD ["/bin/bash"]
