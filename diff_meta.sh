#!/bin/sh

diff --old-group-format='d%dF,%dn
' --new-group-format='a%dF,%dN
' --unchanged-group-format='' --changed-group-format='c%dF,a%dN,d%dn
'  $1 $2

