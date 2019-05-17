FROM haskell:8.4.4

EXPOSE 3000

WORKDIR /app

RUN stack update

COPY ./stack.yaml ./package.yaml /app/
RUN stack install --only-dependencies -j4

COPY . /app/
RUN stack install

CMD ["abtester"]
