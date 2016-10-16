diff $expected $actual
if [ $? -ne 0 ]; then
  echo "test failed" >&2
  return 1
fi
echo "ok"
