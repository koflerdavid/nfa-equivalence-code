# The toplevel directory of the project must be submitted as build context to Docker

FROM fpco/stack-build:lts-9.9 as builder

ENV REF=master

# Build the executable for the website and place it in /dist
RUN git clone https://github.com/koflerdavid/nfa-equivalence-code /app/nfa-equivalence-code \
 && cd /app/nfa-equivalence-code \
 && git checkout $REF \
 && mkdir /out \
 && stack setup \
 && stack --local-bin-path /dist install

# /output will contain everything that the final image shall contain
# It is set up in the builder because the final image won't have coreutils

# chmod, chown, and sh are required by the final image during the build
# Some shared libraries are required, which is taken care of by the next instruction
RUN mkdir -p /output/bin \
 && cp /bin/chmod /bin/chown /bin/rm /bin/sh /output/bin

# Copy shared libraries required by programs in /bin
RUN mkdir -p \
       /output/lib/x86_64-linux-gnu \
       /output/lib64 \
       /output/usr/lib/x86_64-linux-gnu \
 && cp /lib/x86_64-linux-gnu/libz.so.1 \
       /lib/x86_64-linux-gnu/librt.so.1 \
       /lib/x86_64-linux-gnu/libutil.so.1 \
       /lib/x86_64-linux-gnu/libdl.so.2 \
       /lib/x86_64-linux-gnu/libm.so.6 \
       /lib/x86_64-linux-gnu/libpthread.so.0 \
       /lib/x86_64-linux-gnu/libgcc_s.so.1 \
       /lib/x86_64-linux-gnu/libc.so.6 /output/lib/x86_64-linux-gnu \
 && cp /usr/lib/x86_64-linux-gnu/libgmp.so.10 /output/usr/lib/x86_64-linux-gnu \
 && cp /lib64/ld-linux-x86-64.so.2 /output/lib64

# Copy the application
RUN mkdir -p  /output/var/log/www /output/var/www/website \
 && cp /dist/website /output/bin \
 && cp -r /app/nfa-equivalence-code/website/static /output/var/www/website \
 && touch /output/var/log/www/access.log /output/var/log/www/error.log

FROM scratch

COPY --from=builder /output /

# With `userns-remap` enabled, this can only be done in the final container.
# The goal is to restrict down the resulting container as much as possible.
# The server process must not be able to write anywhere, except into the logfiles.
# After doing so, the utilities have to be removed from the image as well.
RUN chmod -R -w /bin /etc /lib /usr /var/www /var/log/www \
 && chmod +w /var/log/www/access.log /var/log/www/error.log \
 && chown -R 33:33 /var/log/www/access.log /var/log/www/error.log \
 && rm /bin/chmod /bin/chown /bin/rm /bin/sh

USER 33:33

WORKDIR /var/www
EXPOSE 8000

ENTRYPOINT ["/bin/website"]
CMD ["--port", \
     "8000", \
     "--access-log=/var/log/www/access.log", \
     "--error-log=/var/log/www/error.log" \
 ]
