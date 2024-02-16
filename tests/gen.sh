DB=db
FILES=files

for file in $FILES/*; do
   folder=$(echo $file | awk -F "/" '{print substr($NF,1,1)}')
   mkdir -pv "$DB/$folder"
   name=$(basename "$file")
   extension="${name##*.}"
   name=${name%.*}

   if [ "$extension" = "hex" ]; then
      echo "$folder/$name generated"
      LC_ALL=C awk -f hex2bin.awk "$file" > "$DB/$folder/$name"
   elif [ "$extension" = "cap" ]; then
      echo "$folder/$name generated"
      tic "$file" -o "$DB"
   fi
done
