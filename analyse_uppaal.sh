#  !/bin/sh
if [ -e systems_generation/uppaal_files/system_1_0.xta ]; then
    rm systems_generation/uppaal_files/*.xta
fi
mv systems_generation/uppaal_files/* rt_to_xta/examples/
cd rt_to_xta/
dune build
for fic in examples/*.rt
do
    fic_no_ext=$(basename $fic .rt)
    ./_build/default/bin/main.exe "./examples/$fic_no_ext" > /dev/null
done
mv examples/* ../systems_generation/uppaal_files/
cd $OLDPWD

for fic in systems_generation/uppaal_files/*.xta
do
    /usr/bin/time -f "%e, %M" ./uppaal-5.1.0-beta5-linux64/bin/verifyta.sh -T -C -q "$fic" 
done