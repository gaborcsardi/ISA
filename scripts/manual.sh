#! /bin/sh

PNAME=$1
VERSION=$2

cp ${PNAME}_${VERSION}.tar.gz /tmp && cd /tmp && 
tar xzf ${PNAME}_${VERSION}.tar.gz && cd - && 
cp /tmp/${PNAME}/html/* homepage/manuals/${PNAME}/ && 
sed 's/\.\.\/\.\.\/doc\/html\///g' <homepage/manuals/${PNAME}/00Index.html | 
sed 's/\.\.\/\.\.\//..\//' | 
grep -v "^<h2>User Guides" | 
grep -v "^Read <a href" >/tmp/a.html && 
mv /tmp/a.html homepage/manuals/${PNAME}/00Index.html && 
touch homepage/manuals/${PNAME}/.stamp
