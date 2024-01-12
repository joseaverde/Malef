for file in files/*; do
   LC_ALL=C awk -f hex2bin.awk $file
done
