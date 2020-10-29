FROM fpco/stack-build
RUN mkdir /app
COPY . /app
WORKDIR /app
RUN stack build
RUN stack install
ENTRYPOINT ["avtor-scotty-exe"]