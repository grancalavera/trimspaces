mkdir -p .tmp 
rm .tmp/* 
cp fixtures/with-spaces.txt .tmp/with-spaces.txt 
runhugs Main.hs .tmp/with-spaces.txt && \ 
echo "diff results" 
diff fixtures/with-spaces.txt .tmp/with-spaces.txt 
echo "cat results"
cat .tmp/with-spaces.txt
