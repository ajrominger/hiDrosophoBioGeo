# script to unpack spatial data archives

for k in *.rar
do
  unrar x $k;
  rm $k
done
