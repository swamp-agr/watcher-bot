#!/bin/bash
set -eux
cwd=`pwd`
trap "cd ${cwd}" EXIT

mkdir self-signed
cd self-signed
openssl genrsa -out key.pem 4096
openssl req -new -key key.pem  -out certificate.csr
openssl x509 -req -in certificate.csr -signkey key.pem -out certificate.pem
