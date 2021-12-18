echo "source: $1"; 
source="$1"

basename=`echo $1 | sed 's/.*\\///
                        s/.mb//'`

./marble.native $source > ${basename}.ll
llc -relocation-model=pic ${basename}.ll > ${basename}.s
cc -o ${basename}.exe ${basename}.s matrix_helper.o
./${basename}.exe

generatedfiles="$generatedfiles ${basename}.ll ${basename}.s ${basename}.exe ${basename}.out"
rm -f $generatedfiles