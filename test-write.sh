./test-pre.sh
runhugs Main.hs fixtures/with-spaces.txt .tmp/with-spaces && \
./test-post.sh

