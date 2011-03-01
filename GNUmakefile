CONFIGURE_ARGS = -O0 --enable-tests
#RUN_COMMAND = time ./dist/build/test-strict-bitstream-O0/test-strict-bitstream-O0
#RUN_COMMAND = time ./dist/build/test-strict-bitstream-O2/test-strict-bitstream-O2
#RUN_COMMAND = time ./dist/build/test-lazy-bitstream-O0/test-lazy-bitstream-O0
#RUN_COMMAND = time ./dist/build/test-lazy-bitstream-O2/test-lazy-bitstream-O2
RUN_COMMAND = time ./dist/build/test-strict-bitstream/test-strict-bitstream
#RUN_COMMAND = time ./dist/build/test-lazy-bitstream/test-lazy-bitstream

include cabal-package.mk

update-web-pages: doc ditz
	rsync -av --delete \
		dist/doc/html/bitstream/ \
		www@nem.cielonegro.org:static.cielonegro.org/htdocs/doc/bitstream
	rsync -av --delete \
		dist/ditz/ \
		www@nem.cielonegro.org:static.cielonegro.org/htdocs/ditz/bitstream
